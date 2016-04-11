import data.fin

open nat fin

-- * Finite fields and groups

--constant (q : ℕ)

definition fq := fin q
notation `𝓕` := fq

definition fq.comm_ring [instance] : comm_ring 𝓕 := sorry
definition fq.fintype [instance]   : fintype 𝓕   := sorry
definition fq.inhabited [instance] : inhabited 𝓕 := sorry
definition fq.decidable_eq [instance] : decidable_eq 𝓕 := sorry

structure grp := (dlog : 𝓕)
notation `𝓖` := grp

notation `⟦` x `⟧` := grp.mk x
