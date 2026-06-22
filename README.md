# Philox

Description
- **Purpose:** Fortran implementation of the Philox family of counter-based pseudorandom number generators. Provides stateless, reproducible RNG blocks and utilities for mapping raw 64-bit words into `C_DOUBLE` double-precision uniform variates.
- **Intended users:** Scientific and HPC Fortran developers who need parallel-friendly, reproducible RNG streams with explicit counter/key control.

---
## Note on AI Assistance

This module was produced with the assistance of Microsoft Copilot. 

I endeavoured to match against [random123](https://github.com/DEShawResearch/random123) which implements the algorithm in [Parallel random numbers: as easy as 1, 2, 3, Salmon, Moraes, Dror & Shaw](http://dl.acm.org/citation.cfm?doid=2063405). The tests use the `ISO_C_BINDING` features of Fortran to directly compare to the `random123` library using the same keys.

__No Guarantee__ is made of correctness or suitability (but it is possibly ok).

---

Features
- **Philox Variants:** Implementations for 2x32, 4x32, 2x64, and 4x64 variants (round and logic modules included).
- **Stateless Counter/Key API:** 256-bit little-endian counter (4×64-bit) plus 128-bit key (2×64-bit) model; no global RNG state.
- **Unit-interval Mappings:** Multiple mappings with inclusive/exclusive endpoint semantics (`_cc`, `_co`, `_oc`, `_oo`) and extraction of the top 53 bits for double precision.
- **Vectorized Fill:** Efficient array-fill routines for bulk sampling.
- **Portable Arithmetic Helpers:** Multiprecision helpers (`mulhilo32`, `mulhilo64`) and explicit masking/shift logic for bit-exact behavior across platforms.
- **FPM-ready:** Project configured for the Fortran Package Manager (`fpm`) with `fpm.toml`.

Repository Structure
- `src/` — Fortran source files implementing types, constants, rounds, logic, and uniform mapping utilities.
- `app/` — Example/demo programs (scalar and vector demos).
- `test/` — Tests and reference C headers (random123) — useful for validation (not required to build).

Build
- Requires a Fortran compiler supporting modern Fortran (free source form, ISO C binding).
- Build with `fpm` (recommended):
```bash
fpm build
```
- Run example/demo executables (auto-executables enabled):
```bash
fpm run demo_scalar_uniform
fpm run demo_vector_uniform
```

Quick Start (example)
- Basic scalar draw in pseudo-code:
	- Prepare a 4-element counter and 2-element key (64-bit integers).
	- Choose an index `idx` (1-based) to select a word from the stream.
	- Call a mapping function to get a double in [a, b] or (a, b) as required.

- Example usage pattern (conceptual):
```
counter = [c1, c2, c3, c4]
key = [k1, k2]
idx = 1
x = philox_uniform_cc(counter, key, idx, a, b)   ! [a,b] inclusive endpoints mapping
```

Worked Fortran Example
This example shows a minimal, copy-paste-ready Fortran program that builds a counter/key, fills an array of 8 doubles using `philox_fill_uniform_cc`, and prints the results. Save as `app/example_fill.f90` or compile alongside the project sources.

```fortran
program example_fill
	use iso_c_binding, only: c_double
	use philox, only: philox_fill_uniform_cc, C64
	implicit none
	integer(kind=C64) :: counter(4)
	integer(kind=C64) :: key(2)
	real(kind=c_double) :: a, b
	real(kind=c_double), allocatable :: x(:)

	allocate(x(8))
	! Example counter/key (choose deterministic values for reproducibility)
	counter = [0_C64, 0_C64, 0_C64, 1_C64]
	key = [12345_C64, 67890_C64]
	a = 0.0_c_double
	b = 1.0_c_double

	call philox_fill_uniform_cc(counter, key, 1_c_int64_t, a, b, x)

	print *, 'Generated values:'
	print *, x

	deallocate(x)
end program example_fill
```
Which produces output (Using `ifx 2026.0.0 20260331`:
```
 Generated values:
  0.270789022090602       0.753506309615939       0.624217905781577     
  0.548468978471349       0.239322725563122       0.340372667868520     
  8.239840206884387E-002  0.775633074515794     
```

API Summary
- **High-level sampling**
	- `philox_uniform_cc`, `philox_uniform_co`, `philox_uniform_oc`, `philox_uniform_oo` — scalar samples with endpoint semantics.
	- `philox_fill_uniform_cc`, `philox_fill_uniform_co`, `philox_fill_uniform_oc`, `philox_fill_uniform_oo` — fill arrays starting at a block index.
- **Low-level helpers**
	- `word_from_index(counter, key, idx)` — produce the 64-bit word for a given 1-based index.
	- `counter_add_blocks_4x64(counter, nblocks)` — advance a 4×64-bit counter by block counts.
	- `philox_u01_*` — unit-interval mapping primitives.
	- `mulhilo32`, `mulhilo64` — precise multiply-high/low helpers in `philox_common`.

Design Notes
- Counter arithmetic is implemented as little-endian 256-bit represented as four 64-bit words; block indexing is 4 words per block.
- Unit-interval mappings use top 53 bits to produce IEEE double-precision values with selectable inclusive/exclusive endpoint behavior.
- Stateless design enables easy, deterministic parallel sampling: partition counters/indices across workers to avoid overlap.

Testing & Validation
- Tests and reference code are available under `test/` (includes random123 reference headers and Fortran tests). Use those for correctness verification and platform comparisons.
- Tests may be run using `fpm test`
- TODO: Refactor to use i.e. `test-drive`

Use in other Fortran projects
- Use this package in other Fortran projects using `fpm` by adding the lines  below to your `fpm.toml`. Then include `use_philox`
```
[dependencies]
philox = { git = "https://github.com/RJaBi/Philox_Fortran"}
```


random123 Sources files
- In order to compile the tests, the source files from [random123](https://github.com/DEShawResearch/random123) are included in `test/random123`. The corresponding license is placed in that directory.

Contributing
- Please open issues or pull requests for bug fixes, documentation improvements, or feature requests.

License
- See `fpm.toml` for license metadata.
- MIT
