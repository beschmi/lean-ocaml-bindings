import data.string data.list algebra.field data.real

abbreviation Ty := Type₁

-- * Fixed prelude
-- ** Expressions
-- *** Types

constant G  : Ty
constant Fq : Ty

-- *** Operators and axioms for G

namespace grp
  constant one  : G
  constant gen  : G
  constant exp  : G → Fq → G
  constant dlog : G → Fq
  constant mul  : G → G → G
  constant inv  : G → G
end grp

notation a ^ b    := grp.exp a b
notation a * b    := grp.mul a b
notation a⁻¹      := grp.inv a
abbreviation g    := grp.gen
notation  1       := grp.one
abbreviation dlog := grp.dlog

constant grp.mul_assoc    : ∀ (a b c : G), (a * b) * c = a * (b * c)
constant grp.one_mul      : ∀ (a : G),     1 * a = a
constant grp.mul_left_inv : ∀ (a : G),     (a⁻¹) * a = 1
constant grp.mul_comm     : ∀ (a b : G),   a * b = b * a
-- print comm_group -- the above axioms are given here
-- FIXME: add axioms for power

definition grp.mul_one : ∀ (a : G), a * 1 = a :=
begin intros a, rewrite [ grp.mul_comm, grp.one_mul] end

-- *** Operators and axioms for Fq

namespace fq
  constant add  : Fq → Fq → Fq
  constant zero : Fq
  constant neg  : Fq → Fq
  constant mul  : Fq → Fq → Fq
  constant one  : Fq
  constant inv  : Fq → Fq
end fq

notation a + b := fq.add a b
notation 0     := fq.zero
notation -a    := fq.neg a
notation a * b := fq.mul a b
notation 1     := fq.one
postfix ⁻¹     := fq.inv

constant fq.add_assoc      : ∀ (a b c : Fq), (a + b) + c = a + (b + c)
constant fq.add_zero       : ∀ (a : Fq),     a + 0 = a
constant fq.add_left_inv   : ∀ (a : Fq),     (- a) + a = 0
constant fq.add_comm       : ∀ (a b : Fq),   a + b = b + a
constant fq.mul_assoc      : ∀ (a b c : Fq), (a * b) * c = a * (b * c)
constant fq.one_mul        : ∀ (a : Fq),     1 * a = a
constant fq.left_distrib   : ∀ (a b c : Fq), a * (b + c) = (a * b) + (a * c)
constant fq.right_distrib  : ∀ (a b c : Fq), (a + b) * c = (a * c) + (b * c)
constant fq.zero_ne_one    :                 fq.zero ≠ fq.one
constant fq.mul_inv_cancel : ∀ {a : Fq},     a ≠ fq.zero → a * a⁻¹ = 1
constant fq.mul_comm       : ∀ (a b : Fq),   a * b = b * a
-- print field -- the above axioms are given here

definition fq.mul_one : ∀ (a : Fq), a * 1 = a :=
begin intros a, rewrite [ fq.mul_comm, fq.one_mul] end

definition fq.inv_mul_cancel : ∀ {a : Fq}, a ≠ fq.zero → a⁻¹ * a = (1 : Fq) :=
begin intros a, rewrite [ fq.mul_comm], exact fq.mul_inv_cancel end
  
-- *** Variables

--inductive var (t : Ty) : Type :=
--Var : string → var t
--open var
--attribute var.name [coercion] 
--definition var.name {t} (v : var t):= var.rec (fun n, n) v
--constant var : Π t : Ty, string → t

-- *** Procedure variables

inductive vproc (Tr Ta : Ty) : Type :=
Vproc : string → vproc Tr Ta

-- ** Distributions

constant Distr : Ty → Ty
constant Unif : Π T : Ty, Distr T 

-- ** Commands and Games

constant Cmd : Ty

-- for both mlet and samp, we must check that first T-arg is
-- (tuple) of var-applications
constant mlet  : Π {T : Ty}, T → T                → Cmd
constant msamp : Π {T : Ty}, T → Distr T          → Cmd
constant mcall : Π {Tr Ta : Ty}, Tr → vproc Tr Ta → Ta → Cmd

constant Proc : Π (Ta Tr : Ty), Ty

constant mk_Proc : Π {Ta Tr : Ty}, Ta → list Cmd → Tr → Proc Ta Tr

notation s ` ⟵ `  e             := mlet s e
notation s ` ⟵$ ` d             := msamp s d
notation sv ` ⟵@ ` sa `|` a     := mcall sv sa a
notation cmds `$` args ` ⇒ ` ret := mk_Proc args cmds ret
notation cmds          ` ⇒ ` ret := mk_Proc unit.star cmds ret

-- ** Procedure applications

inductive papp :=
| Subst : Π {Tr Ta : Ty}, vproc Tr Ta → vproc Tr Ta → papp
| Inst  : Π {Tr Ta : Ty}, vproc Tr Ta → Proc  Tr Ta → papp

constant inst : Π {Ta Tr : Ty}, Proc Ta Tr → papp → Proc Ta Tr

notation `⟪` p1 ` & ` vp `↦` p2 ` ⟫ ` := inst p1 (papp.Inst vp p2)

open real

constant Pr : Π{Ta : Ty}, Proc Ta Prop → Ta → ℝ

abbreviation DFq   := Unif Fq
abbreviation Dbool := Unif bool

-- ** Examples

constant var : Π {t : Ty}, string → t

notation `~`v`:`t := @var t v

constants (x y z : Fq)

definition e1 := (grp.exp g (~"x":Fq) * grp.exp g ((~"y":Fq) + (~"y":Fq)))
