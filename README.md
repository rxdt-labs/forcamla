<p align="center">
  <img src="forcamla_logo.svg" alt="forcamla">
</p>

## What is forcamla?

We start by learning constants in OCaml as ways to have names hold values for us to use later. For instance,

```ocaml
let x = 2
let z = x * x
```
simply defines an integer constant `x` that has the value `2` and we later use `x` again to make `z` have the value of `x * x = 2 * 2 = 4`. But often in programming constants are not enough because we want to change the values these names have after a sequence of instructions. Thus, OCaml has `ref` types for this, observe
```ocaml
let x = ref 2 (* Let x be a *variable* that contains 2 *)
x := !x + 1 (* Update x so it is now x + 1 = 2 + 1 = 3 *)
```
However, even this has it limitations. Suppose we have the following scenario
```ocaml
let x = ref 2
let y = ref 2
let z = ref (!x + !y) (* Make z a variable equal to 2 + 2 = 4 *)
x := 3
```
What happens to `z`? Does `z` change to reflect the fact that `x` changed? Or does `z` stay the same? Most languages (including OCaml) do not update `z` when `x` changes. This might defy your intuition. Even worse, this behavior may be inconvenient to deal with. This is where `forcamla` comes into play!

## Formulae

`forcamla` introduces a `formula` type that behaves similarly to `ref` types *but* formula stay *up-to-date* when their `term`s change. Modifying the example above we have
```ocaml
open Formula (* To use formula *)

let x = t 2 (* Create an integer term called x *)
let y = t 2 (* Create an integer term called y *)
let z = x + y
let () = x =: 3 (* Set x to 3, and z now is 5 *)
```
But it gets even better!

## Event Listeners

Suppose you're making a game. You probably have `player` and maybe it has type `hero` with a bunch of fields including health.
```ocaml
let t = Formula.t
let (=:) = Formula.(=:)
let (=?) = Formula.(=?)
let (!) = Formula.(!)

type hero =
{
  (* A bunuch of fields *)
  health: int formula
}

let player =
{
  (* Assign the fields *)
  health = t 3; (* Give health a value of something, say 3 in this case. *)
}
```
In most games, when the `player.health` is `0` that means "Game Over!". So, we first make a "Game Over!" function:
```ocaml
let game_over () = print_endline "Game Over!"
```
Then, with the power of `forcamla` we can create an *event listener* (see the [MDN docs](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener) to understand what an event listener is) to execute `game_over` when the player health is `0`. Observe
```ocaml
let () = when_satisfied (player.health =? 0) game_over
```

`=?` is used to check if an integer formula is equal to another (integer) formula. This reads as: "When `player.health` is `0` fire the function `game_over`". Therfore, if we run:

```ocaml
let () = player.health =: !player.health - 1 (* Nothing happens yet! player.health is 2 now. *)
let () = player.health =: !player.health - 1 (* Nothing happens yet! player health is 1 now. *)
let () = player.health =: !player.health - 1 (* Now something happens! player.health is 0 and "Game Over!" is printed to the screen! *)
```

## Documentation

More examples and an API reference can be found [here.](https://rxdt-labs.github.io/forcamla/index.html)
