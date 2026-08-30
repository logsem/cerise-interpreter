#!/usr/bin/env python3
"""Enforce explicit types on handwritten named OCaml functions."""

from pathlib import Path
import re
import sys


ROOT = Path(sys.argv[1] if len(sys.argv) > 1 else ".").resolve()
SUFFIXES = {".ml", ".mli", ".mll", ".mly"}
OPENING = {"(": ")", "[": "]", "{": "}"}


def binding_groups(source: str, start: int, stop: int) -> list[tuple[int, int, str]]:
    groups = []
    index = start
    while index < stop:
        while index < stop and source[index].isspace():
            index += 1
        if index >= stop:
            break
        begin = index
        opening = index
        if source[index] == "?" and index + 1 < stop and source[index + 1] == "(":
            opening = index + 1
        elif source[index] == "~":
            labelled = re.match(r"~[a-z_][\w']*:\(", source[index:stop])
            if labelled:
                opening = index + labelled.group(0).rfind("(")
        if source[opening] in OPENING:
            stack = [OPENING[source[opening]]]
            index = opening + 1
            while index < stop and stack:
                if source[index] in OPENING:
                    stack.append(OPENING[source[index]])
                elif source[index] == stack[-1]:
                    stack.pop()
                index += 1
        else:
            while index < stop and not source[index].isspace():
                index += 1
        groups.append((begin, index, source[begin:index]))
    return groups


def binding_equal(source: str, start: int) -> int | None:
    stack = []
    index = start
    while index < len(source):
        character = source[index]
        if character in OPENING:
            stack.append(OPENING[character])
        elif stack and character == stack[-1]:
            stack.pop()
        elif character == "=" and not stack:
            return index
        elif character == "\n" and not stack:
            following = source[index + 1 :]
            if re.match(r"[ \t]*(?:let|and|type|module|open|include|exception|class)\b", following):
                return None
        index += 1
    return None


def source_line(source: str, offset: int) -> int:
    return source.count("\n", 0, offset) + 1


def check_source(path: Path, source: str, label: str) -> list[str]:
    failures = []
    bindings = re.compile(r"(?m)^[ \t]*(?:let|and)[ \t]+(?:rec[ \t]+)?")
    for binding in bindings.finditer(source):
        equal = binding_equal(source, binding.end())
        if equal is None:
            continue
        parts = binding_groups(source, binding.end(), equal)
        if not parts:
            continue
        name = parts[0][2]
        if name in {"module", "type"} or not (
            re.match(r"^[a-z_][\w']*$", name) or re.match(r"^\([!$%&*+\-./:<=>?@^|~ ]+\)$", name)
        ):
            continue
        right_hand_side = source[equal + 1 :].lstrip()
        if re.match(r"(?:function|fun)\b", right_hand_side):
            failures.append(f"{label}:{source_line(source, equal)}: named binding uses function/fun")
        parameters = []
        for part in parts[1:]:
            if part[2].startswith(":"):
                break
            parameters.append(part)
        if not parameters:
            continue
        line = source_line(source, parts[0][0])
        for _, _, parameter in parameters:
            if ":" not in parameter and not parameter.startswith("(type "):
                failures.append(f"{label}:{line}: parameter {parameter!r} lacks an explicit type")
        if not any(part[2].startswith(":") for part in parts[1:]):
            failures.append(f"{label}:{line}: function {name!r} lacks an explicit return type")
    return failures


def header_source(path: Path, source: str) -> str:
    if path.suffix == ".mly":
        match = re.match(r"\s*%\{(.*?)%\}", source, re.S)
    else:
        match = re.match(r"\s*\{(.*?)\}", source, re.S)
    return match.group(1) if match else ""


failures = []
for directory in (ROOT / "lib", ROOT / "src", ROOT / "tests"):
    for path in directory.rglob("*"):
        if path.suffix not in SUFFIXES or "_build" in path.parts:
            continue
        if path.is_relative_to(ROOT / "lib/griotte_extracted/generated"):
            continue
        source = path.read_text()
        relative = str(path.relative_to(ROOT))
        if path.suffix in {".ml", ".mli"}:
            failures.extend(check_source(path, source, relative))
        else:
            failures.extend(check_source(path, header_source(path, source), f"{relative} (header)"))

if failures:
    print("OCaml readability policy violations:", file=sys.stderr)
    for failure in failures:
        print(f"  {failure}", file=sys.stderr)
    sys.exit(1)

print("Handwritten OCaml function annotations and binding forms verified")
