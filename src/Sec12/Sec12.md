# 第 12 章 正規化

> テキストの解答要約はこんな感じで引用表現にする（引用じゃないけど）

## 演習 12.1.1. [$\star$]

E-AppAbs において、項のサイズが減少するとは言えないため。

例えば、 (λc : A→A. c (c (c (c (λz : A. z))))) (λx : A. x) (サイズ 14) を簡約すると、
(λx : A. x) ((λx : A. x) ((λx : A. x) ((λx : A. x) (λz : A. z)))) (サイズ15) となってしまう。

> 適用 t1 t2 が正規化可能であることを示すとき、逆転補題と標準形補題から (λx:T11.t12) v2 の形に簡約できることがわかるが、E-AppAbsを適用すると、代入により元の項よりサイズが大きい項ができる恐れがある（で、その例が先述もの。）

## 演習 12.1.7. [推奨, $\star\star\star$]

### メモ

- 正規化可能：評価が有限ステップで停止
- $R_\mathsf{T}(\mathsf{t})$ : 型 $\mathsf{T}$ を持つ、閉じた項からなる集合
- 定義 12.1.2. 論理述語 R が真なら単純型でも関数型でも停止し、適用されても真理値は変わらない
- 補題 12.1.3. $R_\mathsf{T}(\mathsf{t})$ ならば、 $\sf t$ は停止
- 補題 12.1.4. R の真理値は評価されても変わらない
- 補題 12.1.5. R の真理値は開いた項の閉じたインスタンスの代入でも変わらない
- 定理 12.1.6. [正規化] $\vdash \mathsf{t:T}$ ならば $\mathsf{t}$ は正規化可能
  - 12.1.5. 12.1.3. より
- 演習 12.1.7. ブール値と直積で拡張しても $\vdash \mathsf{t:T}$ ならば $\mathsf{t}$ は正規化可能
  - ブール値： Tにブール値型Bool、型付け規則にT-True/False/Ifがある。
  - 直積： Tに二つ組 {t, t} 射影 t.1 と t.2、型付け規則にT-Pair/Proj1/Proj2がある。

### はい

補題12.1.5. を拡張できれば、定理 12.1.6. の拡張も成り立つ（？）。 よって補題12.1.5. を拡張する。
型付け導出に関する帰納法による。
連続代入は長いので $\sigma_{1..n} \overset{\mathrm{def}}{=} \mathsf{[x_1 \mapsto v_1]\cdots[x_\mathit{n} \mapsto v_\mathit{n}]}$ と略記します…。

- T-True/False の場合、直ちに明らか。
- T-If の場合、 （証明概略、部分項を値まで評価するとRが）
  - $\mathsf{t = if\ t_1\ then\ t_2\ else\ t_3}$ 
    $\mathsf{x_1:T_1,\dots,x_\mathit{n}:T_\mathit{n} \vdash t_1:Bool}$ 
    $\mathsf{x_1:T_1,\dots,x_\mathit{n}:T_\mathit{n} \vdash t_2:S}$ 
    $\mathsf{x_1:T_1,\dots,x_\mathit{n}:T_\mathit{n} \vdash t_3:S}$ 
    $\mathsf{T = S}$ 
  - 帰納法の仮定より、 $R_\mathsf{Bool}(\mathsf{\sigma_{1..n}t_1})$ かつ $R_\mathsf{S}(\mathsf{\sigma_{1..n}t_2})$ かつ $R_\mathsf{S}(\mathsf{\sigma_{1..n}t_3})$ である。
  - $R_\mathsf{Bool}(\sigma_{1..n}\mathsf{t_1})$ と補題12.1.3. から、  $\sigma_{1..n}\mathsf{t_1} \rarr^* \mathsf{v_1}$ となり、 これは $\sf true$ か $\sf false$ かのどちらか。 
    -   $\sf v1 = true$ のとき、 評価導出の最後は E-IfTrue なので $\mathsf{if\ \sigma_{1..n}t_1\ then\ \sigma_{1..n}t_2\ else\ \sigma_{1..n}t_3} \rarr^* \mathsf{\sigma_{1..n}t_2}$ 
         -   $R_\mathsf{S}(\mathsf{\sigma_{1..n}t_2})$ と補題 12.1.4. より、 $R_\mathsf{S}(\mathsf{if\ \sigma_{1..n}t_1\ then\ \sigma_{1..n}t_2\ else\ \sigma_{1..n}t_3})$ となる。
    -   $\sf v1 = false$ のとき、 評価導出の最後は E-IfFalse なので $\mathsf{if\ \sigma_{1..n}t_1\ then\ \sigma_{1..n}t_2\ else\ \sigma_{1..n}t_3} \rarr^* \mathsf{\sigma_{1..n}t_3}$ 
         -   $R_\mathsf{S}(\mathsf{\sigma_{1..n}t_3})$ と補題 12.1.4. より、 $R_\mathsf{S}(\mathsf{if\ \sigma_{1..n}t_1\ then\ \sigma_{1..n}t_2\ else\ \sigma_{1..n}t_3})$ となる。
- T-Pair の場合
  - $\mathsf{t = \{t_1,t_2\}}$ 
    $\mathsf{x_1:T_1,\dots,x_\mathit{n}:T_\mathit{n} \vdash t_1:S_1}$ 
    $\mathsf{x_1:T_1,\dots,x_\mathit{n}:T_\mathit{n} \vdash t_2:S_2}$ 
    $\mathsf{T = S_1 \times S_2}$ 
  - 
- T-Proj1/2 の場合