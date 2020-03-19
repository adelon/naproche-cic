axiom omitted {prop : Prop} : prop


-- Endorelations on a type.
def EndRel (t : Sort _) := t → t → Prop


-- A rewriting system is a type endowed with an endorelation.
structure RewrSys (t : Sort _) := (red : EndRel t)


-- Lean already knows the definition of transitive closure.
#print tc


-- The reflexive transitive closure is given by an inductive definition analogous
-- to the built-in definition of `tc`.
inductive rtc {t} : EndRel t → EndRel t
| base :  ∀ {r : EndRel t} (a b : t),   r a b → rtc r a b
| refl :  ∀ {r : EndRel t} (a : t),     rtc r a a
| trans : ∀ {r : EndRel t} (a b c : t), rtc r a b → rtc r b c → rtc r a c

-- Alternative definition.
inductive rtc' {t} : EndRel t → EndRel t
| refl : ∀ {r : EndRel t} (a : t), rtc' r a a
| step : ∀ {r : EndRel t} (a b c : t), r a b → rtc' r b c → rtc' r a c


lemma rtc_idem {t} (r : EndRel t) : rtc (rtc r) = rtc r := omitted


def covers_bounded_below {t} (r : EndRel t) : Prop :=
  ∀ a b c : t, r a b ∧ r a c → ∃ d : t, rtc r b d ∧ rtc r c d

def loc_confl {t} (R : RewrSys t) : Prop := covers_bounded_below R.red


def confl_Rel {t} (r : EndRel t) : Prop :=
  ∀ a b c : t, rtc r a b ∧ rtc r a c → ∃ d : t, rtc r b d ∧ rtc r c d

def confl {t} (R : RewrSys t) : Prop := confl_Rel R.red


-- Well-foundedness is already defined.
#print acc
#print well_founded

def terminating {t} (R : RewrSys t) : Prop := well_founded (tc R.red)


def nf {t} (R : RewrSys t) (x : t) := ∃ y : t, (rtc R.red x y ∧ ¬ ∃ z : t, R.red y z)


lemma nf_exists {t} (R : RewrSys t) : terminating R → ∀ x : t, nf R x := omitted



-- Lemma (Newman).
-- Any locally confluent terminating rewriting system is confluent.

lemma diamond_lemma {t} : ∀ r : EndRel t, well_founded (tc r) → covers_bounded_below r → confl_Rel r :=
  assume r : EndRel t,
  assume tc_wf : well_founded (tc r),
  assume bound : covers_bounded_below r,
  assume a b c : t,
  let b := bound a b c in
  omitted
  -- TODO: follow the sketch from
  -- https://en.wikipedia.org/wiki/Newman%27s_lemma#Diamond_lemma


lemma newman {t} : ∀ R : RewrSys t, terminating R → loc_confl R → confl R :=
  assume R : RewrSys t,
  assume h_termin : terminating R,
  assume h_loc_confl : loc_confl R,
  diamond_lemma R.red h_termin h_loc_confl
