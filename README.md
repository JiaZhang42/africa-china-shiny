# africa-china-shiny

Standalone Shiny deployment repo for the Africa-China finance explorer.

This repo is intentionally separate from the research repo. It should contain
only:

- the Shiny UI/server code;
- the reduced app bundle exported from the research repo;
- the vendored finance runtime snapshot needed to read that bundle.

## Runtime contract

This app must run from committed artifacts alone. It does not rebuild raw or
clean data at runtime and does not depend on the local `data/` tree inside the
research repo.

## Manual refresh workflow

1. In the research repo, run:

```r
source("scripts/finance_explorer.R")
build_finance_app_bundle()
```

2. Copy the refreshed release artifacts from:

```text
output/finance_app_release/latest/
```

into this repo's `data/` and `R/runtime/` directories.

3. Review the app, commit the changes here, and redeploy.
