# Cerisier paper case studies

These standalone assembly programs adapt the three representative applications
from the [Cerisier paper](https://dl.acm.org/doi/10.1145/3808287). The source of
truth is the official Rocq artifact pinned at
[`57ed584ae17eed308ae0fa554cf0dde9843112c1`](https://github.com/logsem/cerisier/tree/57ed584ae17eed308ae0fa554cf0dde9843112c1/theories/case_studies).
The artifact is distributed under the BSD 3-Clause license; these adaptations
retain that provenance.

| Assembly fixture | Rocq source | Expected halted state |
| --- | --- | --- |
| `secure_outsourced_computation.s` + `.reg` | [`soc/{soc_code,soc_adequacy}.v`](https://github.com/logsem/cerisier/tree/57ed584ae17eed308ae0fa554cf0dde9843112c1/theories/case_studies/soc) | assertion flag `[90] = 0`; `r30 = 0`; enclave counter `1`; identity `3664546399000423895061894810877788924077` |
| `mutual_attestation.s` + `.reg` | [`mutual_attestation/{mutual_attestation_code,mutual_attestation_adequacy}.v`](https://github.com/logsem/cerisier/tree/57ed584ae17eed308ae0fa554cf0dde9843112c1/theories/case_studies/mutual_attestation) | assertion flag `[434] = 0`; `r28 = r29 = r30 = 0`; enclave counter `2`; identities `28586538272572871769842225592882347710580`, `23463451373229852078782113054873498149463` |
| `trusted_sensor_readout.s` + `.reg` | [`memory_readout/{trusted_memory_readout_code,trusted_memory_readout_adequacy}.v`](https://github.com/logsem/cerisier/tree/57ed584ae17eed308ae0fa554cf0dde9843112c1/theories/case_studies/memory_readout) | assertion flag `[178] = 0`; MMIO `[163] = 21`; `r0`–`r5` and `r30` are zero; enclave counter `2`; identities `7182681053998082077296997438663194947164`, `7795339977280813461738391276462452401620` |

The register aliases in the Rocq programs are translated directly (`r_t0` to
`r0`, and so on), their macros are expanded into ordinary assembly, and Rocq's
`Mod` instruction is written as `rem`.

## Concrete compositional identities

The Cerisier backend instantiates the artifact's abstract hash axioms with a
deterministic, non-cryptographic polynomial sequence hash. Hash values encode a
sequence length and a digest modulo `2^127 - 1`; `HashConcat` composes those
states associatively, so hashing a singleton agrees with hashing its word and
hashing appended lists agrees with concatenating their hashes. Addresses and
word constructors use stable structural domain tags. This replaces OCaml's
non-compositional, implementation-dependent `Hashtbl.hash` behavior.

The SOC fixture is a faithful fixed-layout instantiation of `soc_code.v` and
the intended construction in `soc_adequacy.v`, apart from its arbitrary
14-instruction adversary witness. The artifact's
`link_table_region_correct` field accidentally repeats the assertion interval
even though `regions_disjoints` requires those regions to be disjoint; this
fixture uses the intended separate one-cell link table at `[91,92)`. Its `.reg`
file supplies the paper's initial PC and adversary capabilities, with no
bootstrap block. The verifier compares `EStoreId` directly with the concrete
`SOC_HASH` value `3664546399000423895061894810877788924077`. That value was derived in two passes: the final
layout was first assembled with a zero placeholder, executed through the ten
setup steps and `EInit`, and enclave-table entry 0 was read through
`Machine_view`; only the placeholder was then replaced before a fresh run. The
paper assertion routine remains intact: a result mismatch sets its memory flag
to `1` and returns, while sealed-value and identity mismatches take the
verifier's explicit `fail` path.

The mutual fixture translates the paper's 72-word verifier, 167-word A enclave,
136-word B enclave, both 29-instruction `hash_cap_instrs` expansions, and both
nine-instruction assertion expansions literally. Its `.reg` file supplies only
the paper-shaped PC and adversary capabilities. The 36-instruction adversary
witness destroys all broad authority, initializes A and B, and schedules
A → B → A → verifier; it performs no trusted identity enrollment. The embedded
pre-hashes were derived by fresh `EInit` executions whose code capabilities end
immediately before each two-word identity table. Fresh full-code executions
then produced the two verifier identities shown above.

All three adequacy files accidentally repeat the assertion interval for
`link_table_region_correct` while also requiring the assertion and link regions
to be disjoint. These fixtures use the intended separate one-cell link tables.

Trusted sensor readout is likewise a fixed-layout adequacy instantiation. Its
`.reg` file supplies only the paper-shaped initial `PC = (RWX, 0, 39, 0)` and
`r0 = (RWX, 39, 164, 39)` capabilities. The 33-instruction adversary witness
destroys its original broad authority, gives the sensor unique access to the
one-word MMIO cell `[163]`, then initializes and schedules the sensor and client
enclaves before returning their values unchanged to the four-instruction
verifier entry. The verifier and client compare `EStoreId` results directly
with the concrete identities `7795339977280813461738391276462452401620` and
`7182681053998082077296997438663194947164`, respectively.

Those identities were derived in three fresh executions. With both constants
zero, the first `EInit` produced the sensor identity in table entry 0. With that
sensor identity installed and only the client constant zero, the second
`EInit` produced the client identity in entry 1. A final fresh execution with
both constants checks those live entries and halts with MMIO `[163] = 21` and
assertion flag `[178] = 0`. The paper client doubles the sensor value without a
separate `21` check; the verifier's exact assertion macro checks the resulting
`42`, records mismatches in the flag, and returns normally.
