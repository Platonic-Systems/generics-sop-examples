set -xe

pandoc --from=markdown --output=draft.pdf draft.md  \
    -V geometry:"margin=2cm" \
    -V linkcolor:blue \
    -V geometry:a4paper \
    --toc \
    --include-in-header inline_code.tex \
    --highlight-style pygments.theme