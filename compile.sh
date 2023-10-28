cargo run -- $1 tmp.c || exit 1
gcc tmp.c || exit 1
rm tmp.c