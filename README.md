# Kohnert Diagram

This is a program made to look for counterexamples to a friend's conjectures about Kohnert diagrams.

It succeeded.

This was not what we wanted.

## Usage

1. [Install Rust](https://www.rust-lang.org/tools/install)
2. Clone the repository: `git clone https://github.com/HactarCE/kohnert`
3. Run: `cargo run --release`

This program searches for counterexamples to the conjecture that a the poset formed by applying Kohnert moves to a Kohnert diagram is a lattice.

Pass the argument `-i` to only test pairs of elements that cover the same element. (`cargo run --release -- -i`)

Enter a Kohnert diagram, followed by `*` on its own line. For example:

```
xx xx
    x
    x

    x

*
```
