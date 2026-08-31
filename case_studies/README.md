# Case studies

This directory contains runnable assembly programs, organized by the backend they target. The
regression inputs used by the test suite live separately in `../tests/test_files`.

| Directory | Backend | Programs |
| --- | --- | --- |
| [`vanilla`](vanilla) | `vanilla` | Lecture and allocation examples; buffer sharing, sealing, objects, local state, read-only sharing, and support fragments |
| [`ucerise`](ucerise) | `ucerise` | Awkward revocation |
| [`mcerise`](mcerise) | `mcerise` | Downward local-state encapsulation and stack object |
| [`griotte`](griotte) | `griotte` | Counter, locality, key-value store, local-state encapsulation, and switcher examples |
| [`cerisier`](cerisier) | `cerisier` | Secure outsourced computation, mutual attestation, and trusted sensor readout |

Run a program with its matching backend. When a program has a `.reg` companion, pass it with
`--regfile`; it supplies the program's initial register state. For example:

```sh
./interpreter --backend vanilla --regfile case_studies/vanilla/cap_machine_lecture_exercise.reg case_studies/vanilla/cap_machine_lecture_exercise.s
```

The `vanilla/assert.s` and `vanilla/malloc.s` files are reusable support fragments. The
`vanilla/malloc_test.s` program is a standalone, runnable allocation example.

The Cerisier programs are adaptations of the official Rocq artifact. Their detailed provenance,
source links, and expected halted states are documented in the [Cerisier provenance README](cerisier/README.md).
