(* from Isabelle2021-1 src/HOL/Power.thy; BSD license *)

(*  Title:      HOL/Power.thy
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Copyright   1997  University of Cambridge
*)

section \<open>Exponentiation\<close>

theory Power
  imports Num
begin

subsection \<open>Powers for Arbitrary Monoids\<close>

class power = one + times
begin

primrec power :: "'a \<Rightarrow> nat \<Rightarrow> 'a"  (infixr "^" 80)
  where
    power_0: "a ^ 0 = 1"
  | power_Suc: "a ^ Suc n = a * a ^ n"

notation (latex output)
  power ("(_\<^bsup>_\<^esup>)" [1000] 1000)

text \<open>Special syntax for squares.\<close>
abbreviation power2 :: "'a \<Rightarrow> 'a"  ("(_\<^sup>2)" [1000] 999)
  where "x\<^sup>2 \<equiv> x ^ 2"

end

context
  includes lifting_syntax
begin

lemma power_transfer [transfer_rule]:
  \<open>(R ===> (=) ===> R) (^) (^)\<close>
    if [transfer_rule]: \<open>R 1 1\<close>
      \<open>(R ===> R ===> R) (*) (*)\<close>
    for R :: \<open>'a::power \<Rightarrow> 'b::power \<Rightarrow> bool\<close>
  by (simp only: power_def [abs_def]) transfer_prover

end

context monoid_mult
begin

subclass power .

lemma power_one [simp]: "1 ^ n = 1"
  by (induct n) simp_all

lemma power_one_right [simp]: "a ^ 1 = a"
  by simp

lemma power_Suc0_right [simp]: "a ^ Suc 0 = a"
  by simp

lemma power_commutes: "a ^ n * a = a * a ^ n"
  by (induct n) (simp_all add: mult.assoc)

lemma power_Suc2: "a ^ Suc n = a ^ n * a"
  by (simp add: power_commutes)

lemma power_add: "a ^ (m + n) = a ^ m * a ^ n"
  by (induct m) (simp_all add: algebra_simps)

lemma power_mult: "a ^ (m * n) = (a ^ m) ^ n"
  by (induct n) (simp_all add: power_add)

lemma power_even_eq: "a ^ (2 * n) = (a ^ n)\<^sup>2"
  by (subst mult.commute) (simp add: power_mult)

lemma power_odd_eq: "a ^ Suc (2*n) = a * (a ^ n)\<^sup>2"
  by (simp add: power_even_eq)

lemma power_numeral_even: "z ^ numeral (Num.Bit0 w) = (let w = z ^ (numeral w) in w * w)"
  by (simp only: numeral_Bit0 power_add Let_def)

lemma power_numeral_odd: "z ^ numeral (Num.Bit1 w) = (let w = z ^ (numeral w) in z * w * w)"
  by (simp only: numeral_Bit1 One_nat_def add_Suc_right add_0_right
      power_Suc power_add Let_def mult.assoc)

lemma power2_eq_square: "a\<^sup>2 = a * a"
  by (simp add: numeral_2_eq_2)

lemma power3_eq_cube: "a ^ 3 = a * a * a"
  by (simp add: numeral_3_eq_3 mult.assoc)

lemma power4_eq_xxxx: "x^4 = x * x * x * x"
  by (simp add: mult.assoc power_numeral_even)

lemma funpow_times_power: "(times x ^^ f x) = times (x ^ f x)"
proof (induct "f x" arbitrary: f)
  case 0
  then show ?case by (simp add: fun_eq_iff)
next
  case (Suc n)
  define g where "g x = f x - 1" for x
  with Suc have "n = g x" by simp
  with Suc have "times x ^^ g x = times (x ^ g x)" by simp
  moreover from Suc g_def have "f x = g x + 1" by simp
  ultimately show ?case
    by (simp add: power_add funpow_add fun_eq_iff mult.assoc)
qed

lemma power_commuting_commutes:
  assumes "x * y = y * x"
  shows "x ^ n * y = y * x ^n"
proof (induct n)
  case 0
  then show ?case by simp
next
  case (Suc n)
  have "x ^ Suc n * y = x ^ n * y * x"
    by (subst power_Suc2) (simp add: assms ac_simps)
  also have "\<dots> = y * x ^ Suc n"
    by (simp only: Suc power_Suc2) (simp add: ac_simps)
  finally show ?case .
qed

lemma power_minus_mult: "0 < n \<Longrightarrow> a ^ (n - 1) * a = a ^ n"
  by (simp add: power_commutes split: nat_diff_split)

lemma left_right_inverse_power:
  assumes "x * y = 1"
  shows   "x ^ n * y ^ n = 1"
proof (induct n)
  case (Suc n)
  moreover have "x ^ Suc n * y ^ Suc n = x^n * (x * y) * y^n"
    by (simp add: power_Suc2[symmetric] mult.assoc[symmetric])
  ultimately show ?case by (simp add: assms)
qed simp

end

context comm_monoid_mult
begin

lemma power_mult_distrib [algebra_simps, algebra_split_simps, field_simps, field_split_simps, divide_simps]:
  "(a * b) ^ n = (a ^ n) * (b ^ n)"
  by (induction n) (simp_all add: ac_simps)

end

text \<open>Extract constant factors from powers.\<close>
declare power_mult_distrib [where a = "numeral w" for w, simp]
declare power_mult_distrib [where b = "numeral w" for w, simp]

lemma power_add_numeral [simp]: "a^numeral m * a^numeral n = a^numeral (m + n)"
  for a :: "'a::monoid_mult"
  by (simp add: power_add [symmetric])

lemma power_add_numeral2 [simp]: "a^numeral m * (a^numeral n * b) = a^numeral (m + n) * b"
  for a :: "'a::monoid_mult"
  by (simp add: mult.assoc [symmetric])

lemma power_mult_numeral [simp]: "(a^numeral m)^numeral n = a^numeral (m * n)"
  for a :: "'a::monoid_mult"
  by (simp only: numeral_mult power_mult)

context semiring_numeral
begin

lemma numeral_sqr: "numeral (Num.sqr k) = numeral k * numeral k"
  by (simp only: sqr_conv_mult numeral_mult)

lemma numeral_pow: "numeral (Num.pow k l) = numeral k ^ numeral l"
  by (induct l)
    (simp_all only: numeral_class.numeral.simps pow.simps
      numeral_sqr numeral_mult power_add power_one_right)

lemma power_numeral [simp]: "numeral k ^ numeral l = numeral (Num.pow k l)"
  by (rule numeral_pow [symmetric])

end

context semiring_1
begin

lemma of_nat_power [simp]: "of_nat (m ^ n) = of_nat m ^ n"
  by (induct n) simp_all

lemma zero_power: "0 < n \<Longrightarrow> 0 ^ n = 0"
  by (cases n) simp_all

lemma power_zero_numeral [simp]: "0 ^ numeral k = 0"
  by (simp add: numeral_eq_Suc)

lemma zero_power2: "0\<^sup>2 = 0" (* delete? *)
  by (rule power_zero_numeral)

lemma one_power2: "1\<^sup>2 = 1" (* delete? *)
  by (rule power_one)

lemma power_0_Suc [simp]: "0 ^ Suc n = 0"
  by simp

text \<open>It looks plausible as a simprule, but its effect can be strange.\<close>
lemma power_0_left: "0 ^ n = (if n = 0 then 1 else 0)"
  by (cases n) simp_all

end

context semiring_char_0 begin

lemma numeral_power_eq_of_nat_cancel_iff [simp]:
  "numeral x ^ n = of_nat y \<longleftrightarrow> numeral x ^ n = y"
  using of_nat_eq_iff by fastforce

lemma real_of_nat_eq_numeral_power_cancel_iff [simp]:
  "of_nat y = numeral x ^ n \<longleftrightarrow> y = numeral x ^ n"
  using numeral_power_eq_of_nat_cancel_iff [of x n y] by (metis (mono_tags))

lemma of_nat_eq_of_nat_power_cancel_iff[simp]: "(of_nat b) ^ w = of_nat x \<longleftrightarrow> b ^ w = x"
  by (metis of_nat_power of_nat_eq_iff)

lemma of_nat_power_eq_of_nat_cancel_iff[simp]: "of_nat x = (of_nat b) ^ w \<longleftrightarrow> x = b ^ w"
  by (metis of_nat_eq_of_nat_power_cancel_iff)

end

context comm_semiring_1
begin

text \<open>The divides relation.\<close>

lemma le_imp_power_dvd:
  assumes "m \<le> n"
  shows "a ^ m dvd a ^ n"
proof
  from assms have "a ^ n = a ^ (m + (n - m))" by simp
  also have "\<dots> = a ^ m * a ^ (n - m)" by (rule power_add)
  finally show "a ^ n = a ^ m * a ^ (n - m)" .
qed

lemma power_le_dvd: "a ^ n dvd b \<Longrightarrow> m \<le> n \<Longrightarrow> a ^ m dvd b"
  by (rule dvd_trans [OF le_imp_power_dvd])

lemma dvd_power_same: "x dvd y \<Longrightarrow> x ^ n dvd y ^ n"
  by (induct n) (auto simp add: mult_dvd_mono)

lemma dvd_power_le: "x dvd y \<Longrightarrow> m \<ge> n \<Longrightarrow> x ^ n dvd y ^ m"
  by (rule power_le_dvd [OF dvd_power_same])

lemma dvd_power [simp]:
  fixes n :: nat
  assumes "n > 0 \<or> x = 1"
  shows "x dvd (x ^ n)"
  using assms
proof
  assume "0 < n"
  then have "x ^ n = x ^ Suc (n - 1)" by simp
  then show "x dvd (x ^ n)" by simp
next
  assume "x = 1"
  then show "x dvd (x ^ n)" by simp
qed

end

context semiring_1_no_zero_divisors
begin

subclass power .

lemma power_eq_0_iff [simp]: "a ^ n = 0 \<longleftrightarrow> a = 0 \<and> n > 0"
  by (induct n) auto

lemma power_not_zero: "a \<noteq> 0 \<Longrightarrow> a ^ n \<noteq> 0"
  by (induct n) auto

lemma zero_eq_power2 [simp]: "a\<^sup>2 = 0 \<longleftrightarrow> a = 0"
  unfolding power2_eq_square by simp

end

context ring_1
begin

lemma power_minus: "(- a) ^ n = (- 1) ^ n * a ^ n"
proof (induct n)
  case 0
  show ?case by simp
next
  case (Suc n)
  then show ?case
    by (simp del: power_Suc add: power_Suc2 mult.assoc)
qed

lemma power_minus': "NO_MATCH 1 x \<Longrightarrow> (-x) ^ n = (-1)^n * x ^ n"
  by (rule power_minus)

lemma power_minus_Bit0: "(- x) ^ numeral (Num.Bit0 k) = x ^ numeral (Num.Bit0 k)"
  by (induct k, simp_all only: numeral_class.numeral.simps power_add
    power_one_right mult_minus_left mult_minus_right minus_minus)

lemma power_minus_Bit1: "(- x) ^ numeral (Num.Bit1 k) = - (x ^ numeral (Num.Bit1 k))"
  by (simp only: eval_nat_numeral(3) power_Suc power_minus_Bit0 mult_minus_left)

lemma power2_minus [simp]: "(- a)\<^sup>2 = a\<^sup>2"
  by (fact power_minus_Bit0)

lemma power_minus1_even [simp]: "(- 1) ^ (2*n) = 1"
proof (induct n)
  case 0
  show ?case by simp
next
  case (Suc n)
  then show ?case by (simp add: power_add power2_eq_square)
qed

lemma power_minus1_odd: "(- 1) ^ Suc (2*n) = -1"
  by simp

lemma power_minus_even [simp]: "(-a) ^ (2*n) = a ^ (2*n)"
  by (simp add: power_minus [of a])

end

context ring_1_no_zero_divisors
begin

lemma power2_eq_1_iff: "a\<^sup>2 = 1 \<longleftrightarrow> a = 1 \<or> a = - 1"
  using square_eq_1_iff [of a] by (simp add: power2_eq_square)

end

context idom
begin

lemma power2_eq_iff: "x\<^sup>2 = y\<^sup>2 \<longleftrightarrow> x = y \<or> x = - y"
  unfolding power2_eq_square by (rule square_eq_iff)

end

context semidom_divide
begin

lemma power_diff:
  "a ^ (m - n) = (a ^ m) div (a ^ n)" if "a \<noteq> 0" and "n \<le> m"
proof -
  define q where "q = m - n"
  with \<open>n \<le> m\<close> have "m = q + n" by simp
  with \<open>a \<noteq> 0\<close> q_def show ?thesis
    by (simp add: power_add)
qed

end

context algebraic_semidom
begin

lemma div_power: "b dvd a \<Longrightarrow> (a div b) ^ n = a ^ n div b ^ n"
  by (induct n) (simp_all add: div_mult_div_if_dvd dvd_power_same)

lemma is_unit_power_iff: "is_unit (a ^ n) \<longleftrightarrow> is_unit a \<or> n = 0"
  by (induct n) (auto simp add: is_unit_mult_iff)

lemma dvd_power_iff:
  assumes "x \<noteq> 0"
  shows   "x ^ m dvd x ^ n \<longleftrightarrow> is_unit x \<or> m \<le> n"
proof
  assume *: "x ^ m dvd x ^ n"
  {
    assume "m > n"
    note *
    also have "x ^ n = x ^ n * 1" by simp
    also from \<open>m > n\<close> have "m = n + (m - n)" by simp
    also have "x ^ \<dots> = x ^ n * x ^ (m - n)" by (rule power_add)
    finally have "x ^ (m - n) dvd 1"
      by (subst (asm) dvd_times_left_cancel_iff) (insert assms, simp_all)
    with \<open>m > n\<close> have "is_unit x" by (simp add: is_unit_power_iff)
  }
  thus "is_unit x \<or> m \<le> n" by force
qed (auto intro: unit_imp_dvd simp: is_unit_power_iff le_imp_power_dvd)


end

context normalization_semidom_multiplicative
begin

lemma normalize_power: "normalize (a ^ n) = normalize a ^ n"
  by (induct n) (simp_all add: normalize_mult)

lemma unit_factor_power: "unit_factor (a ^ n) = unit_factor a ^ n"
  by (induct n) (simp_all add: unit_factor_mult)

end

context division_ring
begin

text \<open>Perhaps these should be simprules.\<close>
lemma power_inverse [field_simps, field_split_simps, divide_simps]: "inverse a ^ n = inverse (a ^ n)"
proof (cases "a = 0")
  case True
  then show ?thesis by (simp add: power_0_left)
next
  case False
  then have "inverse (a ^ n) = inverse a ^ n"
    by (induct n) (simp_all add: nonzero_inverse_mult_distrib power_commutes)
  then show ?thesis by simp
qed

lemma power_one_over [field_simps, field_split_simps, divide_simps]: "(1 / a) ^ n = 1 / a ^ n"
  using power_inverse [of a] by (simp add: divide_inverse)

end

context field
begin

lemma power_divide [field_simps, field_split_simps, divide_simps]: "(a / b) ^ n = a ^ n / b ^ n"
  by (induct n) simp_all

end


subsection \<open>Exponentiation on ordered types\<close>

context linordered_semidom
begin

lemma zero_less_power [simp]: "0 < a \<Longrightarrow> 0 < a ^ n"
  by (induct n) simp_all

lemma zero_le_power [simp]: "0 \<le> a \<Longrightarrow> 0 \<le> a ^ n"
  by (induct n) simp_all

lemma power_mono: "a \<le> b \<Longrightarrow> 0 \<le> a \<Longrightarrow> a ^ n \<le> b ^ n"
  by (induct n) (auto intro: mult_mono order_trans [of 0 a b])

lemma one_le_power [simp]: "1 \<le> a \<Longrightarrow> 1 \<le> a ^ n"
  using power_mono [of 1 a n] by simp

lemma power_le_one: "0 \<le> a \<Longrightarrow> a \<le> 1 \<Longrightarrow> a ^ n \<le> 1"
  using power_mono [of a 1 n] by simp

lemma power_gt1_lemma:
  assumes gt1: "1 < a"
  shows "1 < a * a ^ n"
proof -
  from gt1 have "0 \<le> a"
    by (fact order_trans [OF zero_le_one less_imp_le])
  from gt1 have "1 * 1 < a * 1" by simp
  also from gt1 have "\<dots> \<le> a * a ^ n"
    by (simp only: mult_mono \<open>0 \<le> a\<close> one_le_power order_less_imp_le zero_le_one order_refl)
  finally show ?thesis by simp
qed

lemma power_gt1: "1 < a \<Longrightarrow> 1 < a ^ Suc n"
  by (simp add: power_gt1_lemma)

lemma one_less_power [simp]: "1 < a \<Longrightarrow> 0 < n \<Longrightarrow> 1 < a ^ n"
  by (cases n) (simp_all add: power_gt1_lemma)

lemma power_le_imp_le_exp:
  assumes gt1: "1 < a"
  shows "a ^ m \<le> a ^ n \<Longrightarrow> m \<le> n"
proof (induct m arbitrary: n)
  case 0
  show ?case by simp
next
  case (Suc m)
  show ?case
  proof (cases n)
    case 0
    with Suc have "a * a ^ m \<le> 1" by simp
    with gt1 show ?thesis
      by (force simp only: power_gt1_lemma not_less [symmetric])
  next
    case (Suc n)
    with Suc.prems Suc.hyps show ?thesis
      by (force dest: mult_left_le_imp_le simp add: less_trans [OF zero_less_one gt1])
  qed
qed

lemma of_nat_zero_less_power_iff [simp]: "of_nat x ^ n > 0 \<longleftrightarrow> x > 0 \<or> n = 0"
  by (induct n) auto

text \<open>Surely we can strengthen this? It holds for \<open>0<a<1\<close> too.\<close>
lemma power_inject_exp [simp]:
  \<open>a ^ m = a ^ n \<longleftrightarrow> m = n\<close> if \<open>1 < a\<close>
  using that by (force simp add: order_class.order.antisym power_le_imp_le_exp)

text \<open>
  Can relax the first premise to \<^term>\<open>0<a\<close> in the case of the
  natural numbers.
\<close>
lemma power_less_imp_less_exp: "1 < a \<Longrightarrow> a ^ m < a ^ n \<Longrightarrow> m < n"
  by (simp add: order_less_le [of m n] less_le [of "a^m" "a^n"] power_le_imp_le_exp)

lemma power_strict_mono [rule_format]: "a < b \<Longrightarrow> 0 \<le> a \<Longrightarrow> 0 < n \<longrightarrow> a ^ n < b ^ n"
  by (induct n) (auto simp: mult_strict_mono le_less_trans [of 0 a b])

lemma power_mono_iff [simp]:
  shows "\<lbrakk>a \<ge> 0; b \<ge> 0; n>0\<rbrakk> \<Longrightarrow> a ^ n \<le> b ^ n \<longleftrightarrow> a \<le> b"
  using power_mono [of a b] power_strict_mono [of b a] not_le by auto

text\<open>Lemma for \<open>power_strict_decreasing\<close>\<close>
lemma power_Suc_less: "0 < a \<Longrightarrow> a < 1 \<Longrightarrow> a * a ^ n < a ^ n"
  by (induct n) (auto simp: mult_strict_left_mono)

lemma power_strict_decreasing [rule_format]: "n < N \<Longrightarrow> 0 < a \<Longrightarrow> a < 1 \<longrightarrow> a ^ N < a ^ n"
proof (induct N)
  case 0
  then show ?case by simp
next
  case (Suc N)
  then show ?case
    apply (auto simp add: power_Suc_less less_Suc_eq)
    apply (subgoal_tac "a * a^N < 1 * a^n")
     apply simp
    apply (rule mult_strict_mono)
       apply auto
    done
qed

text \<open>Proof resembles that of \<open>power_strict_decreasing\<close>.\<close>
lemma power_decreasing: "n \<le> N \<Longrightarrow> 0 \<le> a \<Longrightarrow> a \<le> 1 \<Longrightarrow> a ^ N \<le> a ^ n"
proof (induct N)
  case 0
  then show ?case by simp
next
  case (Suc N)
  then show ?case
    apply (auto simp add: le_Suc_eq)
    apply (subgoal_tac "a * a^N \<le> 1 * a^n")
     apply simp
    apply (rule mult_mono)
       apply auto
    done
qed

lemma power_decreasing_iff [simp]: "\<lbrakk>0 < b; b < 1\<rbrakk> \<Longrightarrow> b ^ m \<le> b ^ n \<longleftrightarrow> n \<le> m"
  using power_strict_decreasing [of m n b]
  by (auto intro: power_decreasing ccontr)

lemma power_strict_decreasing_iff [simp]: "\<lbrakk>0 < b; b < 1\<rbrakk> \<Longrightarrow> b ^ m < b ^ n \<longleftrightarrow> n < m"
  using power_decreasing_iff [of b m n] unfolding le_less
  by (auto dest: power_strict_decreasing le_neq_implies_less)

lemma power_Suc_less_one: "0 < a \<Longrightarrow> a < 1 \<Longrightarrow> a ^ Suc n < 1"
  using power_strict_decreasing [of 0 "Suc n" a] by simp

text \<open>Proof again resembles that of \<open>power_strict_decreasing\<close>.\<close>
lemma power_increasing: "n \<le> N \<Longrightarrow> 1 \<le> a \<Longrightarrow> a ^ n \<le> a ^ N"
proof (induct N)
  case 0
  then show ?case by simp
next
  case (Suc N)
  then show ?case
    apply (auto simp add: le_Suc_eq)
    apply (subgoal_tac "1 * a^n \<le> a * a^N")
     apply simp
    apply (rule mult_mono)
       apply (auto simp add: order_trans [OF zero_le_one])
    done
qed

text \<open>Lemma for \<open>power_strict_increasing\<close>.\<close>
lemma power_less_power_Suc: "1 < a \<Longrightarrow> a ^ n < a * a ^ n"
  by (induct n) (auto simp: mult_strict_left_mono less_trans [OF zero_less_one])

lemma power_strict_increasing: "n < N \<Longrightarrow> 1 < a \<Longrightarrow> a ^ n < a ^ N"
proof (induct N)
  case 0
  then show ?case by simp
next
  case (Suc N)
  then show ?case
    apply (auto simp add: power_less_power_Suc less_Suc_eq)
    apply (subgoal_tac "1 * a^n < a * a^N")
     apply simp
    apply (rule mult_strict_mono)
    apply (auto simp add: less_trans [OF zero_less_one] less_imp_le)
    done
qed

lemma power_increasing_iff [simp]: "1 < b \<Longrightarrow> b ^ x \<le> b ^ y \<longleftrightarrow> x \<le> y"
  by (blast intro: power_le_imp_le_exp power_increasing less_imp_le)

lemma power_strict_increasing_iff [simp]: "1 < b \<Longrightarrow> b ^ x < b ^ y \<longleftrightarrow> x < y"
  by (blast intro: power_less_imp_less_exp power_strict_increasing)

lemma power_le_imp_le_base:
  assumes le: "a ^ Suc n \<le> b ^ Suc n"
    and "0 \<le> b"
  shows "a \<le> b"
proof (rule ccontr)
  assume "\<not> ?thesis"
  then have "b < a" by (simp only: linorder_not_le)
  then have "b ^ Suc n < a ^ Suc n"
    by (simp only: assms(2) power_strict_mono)
  with le show False
    by (simp add: linorder_not_less [symmetric])
qed

lemma power_less_imp_less_base:
  assumes less: "a ^ n < b ^ n"
  assumes nonneg: "0 \<le> b"
  shows "a < b"
proof (rule contrapos_pp [OF less])
  assume "\<not> ?thesis"
  then have "b \<le> a" by (simp only: linorder_not_less)
  from this nonneg have "b ^ n \<le> a ^ n" by (rule power_mono)
  then show "\<not> a ^ n < b ^ n" by (simp only: linorder_not_less)
qed

lemma power_inject_base: "a ^ Suc n = b ^ Suc n \<Longrightarrow> 0 \<le> a \<Longrightarrow> 0 \<le> b \<Longrightarrow> a = b"
  by (blast intro: power_le_imp_le_base order.antisym eq_refl sym)

lemma power_eq_imp_eq_base: "a ^ n = b ^ n \<Longrightarrow> 0 \<le> a \<Longrightarrow> 0 \<le> b \<Longrightarrow> 0 < n \<Longrightarrow> a = b"
  by (cases n) (simp_all del: power_Suc, rule power_inject_base)

lemma power_eq_iff_eq_base: "0 < n \<Longrightarrow> 0 \<le> a \<Longrightarrow> 0 \<le> b \<Longrightarrow> a ^ n = b ^ n \<longleftrightarrow> a = b"
  using power_eq_imp_eq_base [of a n b] by auto

lemma power2_le_imp_le: "x\<^sup>2 \<le> y\<^sup>2 \<Longrightarrow> 0 \<le> y \<Longrightarrow> x \<le> y"
  unfolding numeral_2_eq_2 by (rule power_le_imp_le_base)

lemma power2_less_imp_less: "x\<^sup>2 < y\<^sup>2 \<Longrightarrow> 0 \<le> y \<Longrightarrow> x < y"
  by (rule power_less_imp_less_base)

lemma power2_eq_imp_eq: "x\<^sup>2 = y\<^sup>2 \<Longrightarrow> 0 \<le> x \<Longrightarrow> 0 \<le> y \<Longrightarrow> x = y"
  unfolding numeral_2_eq_2 by (erule (2) power_eq_imp_eq_base) simp

lemma power_Suc_le_self: "0 \<le> a \<Longrightarrow> a \<le> 1 \<Longrightarrow> a ^ Suc n \<le> a"
  using power_decreasing [of 1 "Suc n" a] by simp

lemma power2_eq_iff_nonneg [simp]:
  assumes "0 \<le> x" "0 \<le> y"
  shows "(x ^ 2 = y ^ 2) \<longleftrightarrow> x = y"
using assms power2_eq_imp_eq by blast

lemma of_nat_less_numeral_power_cancel_iff[simp]:
  "of_nat x < numeral i ^ n \<longleftrightarrow> x < numeral i ^ n"
  using of_nat_less_iff[of x "numeral i ^ n", unfolded of_nat_numeral of_nat_power] .

lemma of_nat_le_numeral_power_cancel_iff[simp]:
  "of_nat x \<le> numeral i ^ n \<longleftrightarrow> x \<le> numeral i ^ n"
  using of_nat_le_iff[of x "numeral i ^ n", unfolded of_nat_numeral of_nat_power] .

lemma numeral_power_less_of_nat_cancel_iff[simp]:
  "numeral i ^ n < of_nat x \<longleftrightarrow> numeral i ^ n < x"
  using of_nat_less_iff[of "numeral i ^ n" x, unfolded of_nat_numeral of_nat_power] .

lemma numeral_power_le_of_nat_cancel_iff[simp]:
  "numeral i ^ n \<le> of_nat x \<longleftrightarrow> numeral i ^ n \<le> x"
  using of_nat_le_iff[of "numeral i ^ n" x, unfolded of_nat_numeral of_nat_power] .

lemma of_nat_le_of_nat_power_cancel_iff[simp]: "(of_nat b) ^ w \<le> of_nat x \<longleftrightarrow> b ^ w \<le> x"
  by (metis of_nat_le_iff of_nat_power)

lemma of_nat_power_le_of_nat_cancel_iff[simp]: "of_nat x \<le> (of_nat b) ^ w \<longleftrightarrow> x \<le> b ^ w"
  by (metis of_nat_le_iff of_nat_power)

lemma of_nat_less_of_nat_power_cancel_iff[simp]: "(of_nat b) ^ w < of_nat x \<longleftrightarrow> b ^ w < x"
  by (metis of_nat_less_iff of_nat_power)

lemma of_nat_power_less_of_nat_cancel_iff[simp]: "of_nat x < (of_nat b) ^ w \<longleftrightarrow> x < b ^ w"
  by (metis of_nat_less_iff of_nat_power)

end


text \<open>Some @{typ nat}-specific lemmas:\<close>

lemma mono_ge2_power_minus_self:
  assumes "k \<ge> 2" shows "mono (\<lambda>m. k ^ m - m)"
unfolding mono_iff_le_Suc
proof
  fix n
  have "k ^ n < k ^ Suc n" using power_strict_increasing_iff[of k "n" "Suc n"] assms by linarith
  thus "k ^ n - n \<le> k ^ Suc n - Suc n" by linarith
qed

lemma self_le_ge2_pow[simp]:
  assumes "k \<ge> 2" shows "m \<le> k ^ m"
proof (induction m)
  case 0 show ?case by simp
next
  case (Suc m)
  hence "Suc m \<le> Suc (k ^ m)" by simp
  also have "... \<le> k^m + k^m" using one_le_power[of k m] assms by linarith
  also have "... \<le> k * k^m" by (metis mult_2 mult_le_mono1[OF assms])
  finally show ?case by simp
qed

lemma diff_le_diff_pow[simp]:
  assumes "k \<ge> 2" shows "m - n \<le> k ^ m - k ^ n"
proof (cases "n \<le> m")
  case True
  thus ?thesis
    using monoD[OF mono_ge2_power_minus_self[OF assms] True] self_le_ge2_pow[OF assms, of m]
    by (simp add: le_diff_conv le_diff_conv2)
qed auto


context linordered_ring_strict
begin

lemma sum_squares_eq_zero_iff: "x * x + y * y = 0 \<longleftrightarrow> x = 0 \<and> y = 0"
  by (simp add: add_nonneg_eq_0_iff)

lemma sum_squares_le_zero_iff: "x * x + y * y \<le> 0 \<longleftrightarrow> x = 0 \<and> y = 0"
  by (simp add: le_less not_sum_squares_lt_zero sum_squares_eq_zero_iff)

lemma sum_squares_gt_zero_iff: "0 < x * x + y * y \<longleftrightarrow> x \<noteq> 0 \<or> y \<noteq> 0"
  by (simp add: not_le [symmetric] sum_squares_le_zero_iff)

end

context linordered_idom
begin

lemma zero_le_power2 [simp]: "0 \<le> a\<^sup>2"
  by (simp add: power2_eq_square)

lemma zero_less_power2 [simp]: "0 < a\<^sup>2 \<longleftrightarrow> a \<noteq> 0"
  by (force simp add: power2_eq_square zero_less_mult_iff linorder_neq_iff)

lemma power2_less_0 [simp]: "\<not> a\<^sup>2 < 0"
  by (force simp add: power2_eq_square mult_less_0_iff)

lemma power_abs: "\<bar>a ^ n\<bar> = \<bar>a\<bar> ^ n" \<comment> \<open>FIXME simp?\<close>
  by (induct n) (simp_all add: abs_mult)

lemma power_sgn [simp]: "sgn (a ^ n) = sgn a ^ n"
  by (induct n) (simp_all add: sgn_mult)

lemma abs_power_minus [simp]: "\<bar>(- a) ^ n\<bar> = \<bar>a ^ n\<bar>"
  by (simp add: power_abs)

lemma zero_less_power_abs_iff [simp]: "0 < \<bar>a\<bar> ^ n \<longleftrightarrow> a \<noteq> 0 \<or> n = 0"
proof (induct n)
  case 0
  show ?case by simp
next
  case Suc
  then show ?case by (auto simp: zero_less_mult_iff)
qed

lemma zero_le_power_abs [simp]: "0 \<le> \<bar>a\<bar> ^ n"
  by (rule zero_le_power [OF abs_ge_zero])

lemma power2_less_eq_zero_iff [simp]: "a\<^sup>2 \<le> 0 \<longleftrightarrow> a = 0"
  by (simp add: le_less)

lemma abs_power2 [simp]: "\<bar>a\<^sup>2\<bar> = a\<^sup>2"
  by (simp add: power2_eq_square)

lemma power2_abs [simp]: "\<bar>a\<bar>\<^sup>2 = a\<^sup>2"
  by (simp add: power2_eq_square)

lemma odd_power_less_zero: "a < 0 \<Longrightarrow> a ^ Suc (2 * n) < 0"
proof (induct n)
  case 0
  then show ?case by simp
next
  case (Suc n)
  have "a ^ Suc (2 * Suc n) = (a*a) * a ^ Suc(2*n)"
    by (simp add: ac_simps power_add power2_eq_square)
  then show ?case
    by (simp del: power_Suc add: Suc mult_less_0_iff mult_neg_neg)
qed

lemma odd_0_le_power_imp_0_le: "0 \<le> a ^ Suc (2 * n) \<Longrightarrow> 0 \<le> a"
  using odd_power_less_zero [of a n]
  by (force simp add: linorder_not_less [symmetric])

lemma zero_le_even_power'[simp]: "0 \<le> a ^ (2 * n)"
proof (induct n)
  case 0
  show ?case by simp
next
  case (Suc n)
  have "a ^ (2 * Suc n) = (a*a) * a ^ (2*n)"
    by (simp add: ac_simps power_add power2_eq_square)
  then show ?case
    by (simp add: Suc zero_le_mult_iff)
qed

lemma sum_power2_ge_zero: "0 \<le> x\<^sup>2 + y\<^sup>2"
  by (intro add_nonneg_nonneg zero_le_power2)

lemma not_sum_power2_lt_zero: "\<not> x\<^sup>2 + y\<^sup>2 < 0"
  unfolding not_less by (rule sum_power2_ge_zero)

lemma sum_power2_eq_zero_iff: "x\<^sup>2 + y\<^sup>2 = 0 \<longleftrightarrow> x = 0 \<and> y = 0"
  unfolding power2_eq_square by (simp add: add_nonneg_eq_0_iff)

lemma sum_power2_le_zero_iff: "x\<^sup>2 + y\<^sup>2 \<le> 0 \<longleftrightarrow> x = 0 \<and> y = 0"
  by (simp add: le_less sum_power2_eq_zero_iff not_sum_power2_lt_zero)

lemma sum_power2_gt_zero_iff: "0 < x\<^sup>2 + y\<^sup>2 \<longleftrightarrow> x \<noteq> 0 \<or> y \<noteq> 0"
  unfolding not_le [symmetric] by (simp add: sum_power2_le_zero_iff)

lemma abs_le_square_iff: "\<bar>x\<bar> \<le> \<bar>y\<bar> \<longleftrightarrow> x\<^sup>2 \<le> y\<^sup>2"
  (is "?lhs \<longleftrightarrow> ?rhs")
proof
  assume ?lhs
  then have "\<bar>x\<bar>\<^sup>2 \<le> \<bar>y\<bar>\<^sup>2" by (rule power_mono) simp
  then show ?rhs by simp
next
  assume ?rhs
  then show ?lhs
    by (auto intro!: power2_le_imp_le [OF _ abs_ge_zero])
qed

lemma power2_le_iff_abs_le:
  "y \<ge> 0 \<Longrightarrow> x\<^sup>2 \<le> y\<^sup>2 \<longleftrightarrow> \<bar>x\<bar> \<le> y"
  by (metis abs_le_square_iff abs_of_nonneg)

lemma abs_square_le_1:"x\<^sup>2 \<le> 1 \<longleftrightarrow> \<bar>x\<bar> \<le> 1"
  using abs_le_square_iff [of x 1] by simp

lemma abs_square_eq_1: "x\<^sup>2 = 1 \<longleftrightarrow> \<bar>x\<bar> = 1"
  by (auto simp add: abs_if power2_eq_1_iff)

lemma abs_square_less_1: "x\<^sup>2 < 1 \<longleftrightarrow> \<bar>x\<bar> < 1"
  using  abs_square_eq_1 [of x] abs_square_le_1 [of x] by (auto simp add: le_less)

lemma square_le_1:
  assumes "- 1 \<le> x" "x \<le> 1"
  shows "x\<^sup>2 \<le> 1"
    using assms
    by (metis add.inverse_inverse linear mult_le_one neg_equal_0_iff_equal neg_le_iff_le power2_eq_square power_minus_Bit0)

end


subsection \<open>Miscellaneous rules\<close>

lemma (in linordered_semidom) self_le_power: "1 \<le> a \<Longrightarrow> 0 < n \<Longrightarrow> a \<le> a ^ n"
  using power_increasing [of 1 n a] power_one_right [of a] by auto

lemma (in power) power_eq_if: "p ^ m = (if m=0 then 1 else p * (p ^ (m - 1)))"
  unfolding One_nat_def by (cases m) simp_all

lemma (in comm_semiring_1) power2_sum: "(x + y)\<^sup>2 = x\<^sup>2 + y\<^sup>2 + 2 * x * y"
  by (simp add: algebra_simps power2_eq_square mult_2_right)

context comm_ring_1
begin

lemma power2_diff: "(x - y)\<^sup>2 = x\<^sup>2 + y\<^sup>2 - 2 * x * y"
  by (simp add: algebra_simps power2_eq_square mult_2_right)

lemma power2_commute: "(x - y)\<^sup>2 = (y - x)\<^sup>2"
  by (simp add: algebra_simps power2_eq_square)

lemma minus_power_mult_self: "(- a) ^ n * (- a) ^ n = a ^ (2 * n)"
  by (simp add: power_mult_distrib [symmetric])
    (simp add: power2_eq_square [symmetric] power_mult [symmetric])

lemma minus_one_mult_self [simp]: "(- 1) ^ n * (- 1) ^ n = 1"
  using minus_power_mult_self [of 1 n] by simp

lemma left_minus_one_mult_self [simp]: "(- 1) ^ n * ((- 1) ^ n * a) = a"
  by (simp add: mult.assoc [symmetric])

end

text \<open>Simprules for comparisons where common factors can be cancelled.\<close>

lemmas zero_compare_simps =
  add_strict_increasing add_strict_increasing2 add_increasing
  zero_le_mult_iff zero_le_divide_iff
  zero_less_mult_iff zero_less_divide_iff
  mult_le_0_iff divide_le_0_iff
  mult_less_0_iff divide_less_0_iff
  zero_le_power2 power2_less_0


subsection \<open>Exponentiation for the Natural Numbers\<close>

lemma nat_one_le_power [simp]: "Suc 0 \<le> i \<Longrightarrow> Suc 0 \<le> i ^ n"
  by (rule one_le_power [of i n, unfolded One_nat_def])

lemma nat_zero_less_power_iff [simp]: "x ^ n > 0 \<longleftrightarrow> x > 0 \<or> n = 0"
  for x :: nat
  by (induct n) auto

lemma nat_power_eq_Suc_0_iff [simp]: "x ^ m = Suc 0 \<longleftrightarrow> m = 0 \<or> x = Suc 0"
  by (induct m) auto

lemma power_Suc_0 [simp]: "Suc 0 ^ n = Suc 0"
  by simp

text \<open>
  Valid for the naturals, but what if \<open>0 < i < 1\<close>? Premises cannot be
  weakened: consider the case where \<open>i = 0\<close>, \<open>m = 1\<close> and \<open>n = 0\<close>.
\<close>

lemma nat_power_less_imp_less:
  fixes i :: nat
  assumes nonneg: "0 < i"
  assumes less: "i ^ m < i ^ n"
  shows "m < n"
proof (cases "i = 1")
  case True
  with less power_one [where 'a = nat] show ?thesis by simp
next
  case False
  with nonneg have "1 < i" by auto
  from power_strict_increasing_iff [OF this] less show ?thesis ..
qed

lemma power_gt_expt: "n > Suc 0 \<Longrightarrow> n^k > k"
  by (induction k) (auto simp: less_trans_Suc n_less_m_mult_n)

lemma less_exp [simp]:
  \<open>n < 2 ^ n\<close>
  by (simp add: power_gt_expt)

lemma power_dvd_imp_le:
  fixes i :: nat
  assumes "i ^ m dvd i ^ n" "1 < i"
  shows "m \<le> n"
  using assms by (auto intro: power_le_imp_le_exp [OF \<open>1 < i\<close> dvd_imp_le])

lemma dvd_power_iff_le:
  fixes k::nat
  shows "2 \<le> k \<Longrightarrow> ((k ^ m) dvd (k ^ n) \<longleftrightarrow> m \<le> n)"
  using le_imp_power_dvd power_dvd_imp_le by force

lemma power2_nat_le_eq_le: "m\<^sup>2 \<le> n\<^sup>2 \<longleftrightarrow> m \<le> n"
  for m n :: nat
  by (auto intro: power2_le_imp_le power_mono)

lemma power2_nat_le_imp_le:
  fixes m n :: nat
  assumes "m\<^sup>2 \<le> n"
  shows "m \<le> n"
proof (cases m)
  case 0
  then show ?thesis by simp
next
  case (Suc k)
  show ?thesis
  proof (rule ccontr)
    assume "\<not> ?thesis"
    then have "n < m" by simp
    with assms Suc show False
      by (simp add: power2_eq_square)
  qed
qed

lemma ex_power_ivl1: fixes b k :: nat assumes "b \<ge> 2"
shows "k \<ge> 1 \<Longrightarrow> \<exists>n. b^n \<le> k \<and> k < b^(n+1)" (is "_ \<Longrightarrow> \<exists>n. ?P k n")
proof(induction k)
  case 0 thus ?case by simp
next
  case (Suc k)
  show ?case
  proof cases
    assume "k=0"
    hence "?P (Suc k) 0" using assms by simp
    thus ?case ..
  next
    assume "k\<noteq>0"
    with Suc obtain n where IH: "?P k n" by auto
    show ?case
    proof (cases "k = b^(n+1) - 1")
      case True
      hence "?P (Suc k) (n+1)" using assms
        by (simp add: power_less_power_Suc)
      thus ?thesis ..
    next
      case False
      hence "?P (Suc k) n" using IH by auto
      thus ?thesis ..
    qed
  qed
qed

lemma ex_power_ivl2: fixes b k :: nat assumes "b \<ge> 2" "k \<ge> 2"
  shows "\<exists>n. b^n < k \<and> k \<le> b^(n+1)"
proof -
  have "1 \<le> k - 1" using assms(2) by arith
  from ex_power_ivl1[OF assms(1) this]
  obtain n where "b ^ n \<le> k - 1 \<and> k - 1 < b ^ (n + 1)" ..
  hence "b^n < k \<and> k \<le> b^(n+1)" using assms by auto
  thus ?thesis ..
qed


subsubsection \<open>Cardinality of the Powerset\<close>

lemma card_UNIV_bool [simp]: "card (UNIV :: bool set) = 2"
  unfolding UNIV_bool by simp

lemma card_Pow: "finite A \<Longrightarrow> card (Pow A) = 2 ^ card A"
proof (induct rule: finite_induct)
  case empty
  show ?case by simp
next
  case (insert x A)
  from \<open>x \<notin> A\<close> have disjoint: "Pow A \<inter> insert x ` Pow A = {}" by blast
  from \<open>x \<notin> A\<close> have inj_on: "inj_on (insert x) (Pow A)"
    unfolding inj_on_def by auto

  have "card (Pow (insert x A)) = card (Pow A \<union> insert x ` Pow A)"
    by (simp only: Pow_insert)
  also have "\<dots> = card (Pow A) + card (insert x ` Pow A)"
    by (rule card_Un_disjoint) (use \<open>finite A\<close> disjoint in simp_all)
  also from inj_on have "card (insert x ` Pow A) = card (Pow A)"
    by (rule card_image)
  also have "\<dots> + \<dots> = 2 * \<dots>" by (simp add: mult_2)
  also from insert(3) have "\<dots> = 2 ^ Suc (card A)" by simp
  also from insert(1,2) have "Suc (card A) = card (insert x A)"
    by (rule card_insert_disjoint [symmetric])
  finally show ?case .
qed


subsection \<open>Code generator tweak\<close>

code_identifier
  code_module Power \<rightharpoonup> (SML) Arith and (OCaml) Arith and (Haskell) Arith

end
