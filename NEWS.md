# hprcc 0.2.0

## Breaking: one task per worker by default (`tasks_max = 1L`)

`create_controller()` gains a `tasks_max` argument, defaulting to `1L`. It was
previously unset, so it took `crew`'s default of `Inf` and a worker kept
accepting targets until it idled out or SLURM killed it at the walltime.

That silently discarded work. A worker would finish target A, start target B, and
be killed mid-B - so B's hours were lost and it had to be redone on a later
worker. Observed on a scRNA-seq pipeline where **all 6 workers** on a 12-hour
SCTransform step ended `FAILED` rather than `COMPLETED`, each part-way through a
second task. It also makes `slurm_walltime_minutes` mean what it says: a
*per-task* limit rather than a per-worker budget shared across an unknown number
of tasks.

Raise it explicitly for many short targets, where process startup dominates and
no single target approaches the walltime.

## Also in this release

`create_controller()`'s singularity invocation now sets `R_LIBS` alongside the
existing `R_LIBS_SITE`, putting the bioc library ahead of the container's baked-in
`/usr/local/lib/R/site-library`. That library ships a stale `rlang` (1.1.6) which
otherwise shadows the newer one and breaks packages requiring >= 1.1.7 with
`namespace 'rlang' 1.1.6 is already loaded`.

## New resource tiers (closes #33)

Two gaps in the catalogue, both sized from measured `MaxRSS` across 1082 crew
workers rather than estimated:

- **`large_mem_2x`** (8 CPUs, 250 GB gemini / 200 GB apollo, 12h) - fills the
  100 GB to 600 GB hole. Measured p99 was 220 GB and the highest per-target peak
  139 GB, so this covers the real tail without leaving the general partition.
  Previously any target over 100 GB had to jump 6x *and* move to `bigmem`.
- **`long`** (4 CPUs, 100 GB, 24h) - for long-running targets that are not
  memory-hungry. Stock walltimes topped out at 480 min on `compute`, so a
  12-hour step was forced onto `large_mem_xl` purely for its duration, dragging a
  600 GB `bigmem` reservation with it and triggering `MaxMemoryPerAccount`
  throttling for the rest of the pipeline. 4 CPUs because the motivating workload
  measured 49.8% of *one* core; many Seurat/Bioconductor steps are effectively
  single-threaded, and over-requesting CPUs inflates the billing weight that
  drives account limits.

`large_mem_2x` is cluster-gated, following the precedent of the GPU checks: node
memory differs between clusters, so a tier that schedules widely on one can be a
scarcity trap on the other. On apollo's `all` partition, 250 GB would exclude the
20 nodes at 239 GB and leave only 10 candidates; 200 GB keeps all 30.

# hprcc 0.1.0

- Add `large_mem_xl` controller (8 CPUs, 600 GB, 12h, bigmem) for very large scRNA-seq objects
- Tune `large_mem`: 8 CPUs, 100 GB RAM, 8h wall on compute — uses all cores for glmGamPoi parallelism; drops explicit bigmem since 100 GB fits compute nodes
- Extend wall time for `xlarge` (6→12h) and `huge` (2→12h) on compute partition
- Add Partition column to resource table in README and docs

# hprcc 0.0.4

- Fix SLURM log paths: respect `options(hprcc.slurm_logs = TRUE)` set before `library(hprcc)`
- Fix SLURM log paths: use `getwd()` instead of `here::here()` to avoid wrong project root detection
- Fix SLURM log paths: handle absolute and tilde paths from `tar_path_store()` correctly

# hprcc 0.0.3

- cd to folder containing target store in SLURM script

# hprcc 0.0.2

- bug fixes

# hprcc 0.0.1

- explore_logs() Shiny app with controller recommendations
- moved to <https://cgt.coh.org/hprcc>

# hprcc 0.0.0.9008

- multiple bug fixes

# hprcc 0.0.0.9007

- multiple bug fixes

# hprcc 0.0.0.9006

- update to new crew.cluster syntax

# hprcc 0.0.0.9005

- add support for gpu queue on _Gemini_

# hprcc 0.0.0.9004

- Use .onAttach() instead of .onLoad() by @drejom in <https://github.com/cohmathonc/hprcc/pull/8>
- Fix for gemini login nodes by @drejom in <https://github.com/cohmathonc/hprcc/pull/7>
- added template scripts for Apollo and Gemini in `extdata`

# hprcc 0.0.0.9003

- refactored package options: can be set via options() in _targets.R, environment variables or cluster specific defaults.

# hprcc 0.0.0.9002

- Moved to cohmathonc/hprcc
- package website published (<http://hprcc.coh.org/user-guide/rbioc/hprcc/>)
