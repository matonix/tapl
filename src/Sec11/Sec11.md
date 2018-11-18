# 第 11 章 単純な拡張

> テキストの解答要約はこんな感じで引用表現にする（引用じゃないけど）

## 演習 11.2.1. $[\star\star\star]$

チャーチ数を次のように与える（型は省略）

- ${\sf c_2 = \textrm{λ}s. s\ (s\ unit)}$
- ${\sf c_n = \textrm{λ}s. \underbrace{s (s ... (s}_{n\ times}\ unit\underbrace{) ... )}_{n\ times}}$
  - つまりサイズ $O(n)$

続いて、関数 $\sf power$ を次のように与える

- ${\sf power = \textrm{λ}n. \textrm{λ}m. m\ n}$

最後に項 $\sf t_n$ を次のように与える

- ${\sf t_n = power\ c_2\ c_n}$

これを評価すると、$O(2^n)$ ステップの後、チャーチ数 $\sf c_{2^n}$ となる。 

> 解：漸化式のように定義する。特に再帰部では、1つ前の添字の項をconst化して2回適用する。

定理 11.3.1. [逐次実行は派生形式である]

- 外部言語（表層構文）を内部言語の派生形式（糖衣構文とも）とすることで、証明をシンプルに維持する

## 演習 11.3.2. [$\star$]

型付け規則

$$
\frac{{\sf \_ : Unit} \in \Gamma}{\Gamma \vdash {\sf \_ : Unit}}
$$
評価規則
$$
(\lambda \_ :{\sf Unit.t)\ v \longrightarrow t}
$$
[証明] 

- 型付け規則 T-Var の $\sf x$ を $\_$ に置き換え、型 $\sf T$ を $\sf Unit$ に置き換えればよい。
- 評価規則では、E-AppAbs を上記同様置き換え、代入の項を除去すればよい。

> 解：ワイルドカードの型をUnitに固定しない。また、型付け規則では、T-Absの前提部から x の型を取り除く。

## 演習 11.4.1. [推奨, $\star\star$]

(1) 新しい派生形式
$$
{\sf t\ as\ T} \overset{\mathrm{def}}{=} (\lambda{\sf x:T.x)\ t}
$$
を与えると、脱糖衣した型付け規則は以下になる。
$$
\frac{\Gamma \vdash {\sf t_1:T}}{\Gamma \vdash {\sf (\lambda x:T.x)\ t_1:T}} \quad (\textrm{T-Ascribe})
$$
これは、単純型付きラムダ計算の型付け規則のみで導出できる。
$$
\displaystyle\frac{
	\displaystyle\frac{
		\displaystyle\frac{
            {\sf x:T} \in \Gamma,{\sf x:T}
		}{
            \Gamma,{\sf x:T} \vdash {\sf x:T}
		} \textrm{T-Var}
	}{
        \Gamma \vdash {\sf \lambda x:T.x : T \rightarrow T}
	} \textrm{T-Abs} \quad \Gamma \vdash {\sf t_1:T}
}{
    \Gamma \vdash {\sf (\lambda x:T.x)\ t_1:T}
} \textrm{T-App}
$$

同様に、脱糖衣した評価規則 E-Ascribe は E-AppAbs によって、 E-Ascribe1 は E-App2 によって、それぞれ導出木を構成できる。

(2) E-AscribeEager を純粋型付きラムダ計算の評価規則に脱糖衣できるできるような派生形式は作れない（根拠なし）。
少なくとも、(1) の派生形式は、純粋型付きラムダ計算においてラムダ抽象に項を適用する評価規則がないため、扱うことはできない。

> (2) では、次の脱糖衣を採用すれば実現できる。ただし、定理11.3.1の要求は、高水準の評価ステップと低水準の評価ステップが１対多となるように弱めなければならない（この脱糖衣によって１ステップの評価が2ステップかかるようになるため）。
> $$
> {\sf t\ as\ T} \overset{\mathrm{def}}{=} (\lambda{\sf x:Unit \rightarrow T.x\ unit)\ (\lambda y:Unit.t)}\quad \textrm{where ${\sf y}$ is fresh}
> $$
>

## 演習 11.5.1. [推奨, $\star\star\star$] 

→実装（`Sec11/SimplyTypedLet.hs`）

## 演習 11.5.2. [$\star\star$]

このアイディアは、 $\sf t_2$ 中で複数 $\sf x$ が出現する場合に、もともとの $\sf let$ の定義を用いる場合に比べて評価回数が倍増するという問題がある。

> 1. 評価順を変えてしまう
> 2. well-typed でない項が、評価によって well-typed になるケースがある

## 演習 11.8.1. [$\star \nrightarrow$]

$$
\{\textsf{l}_1=\textsf{v}_1,\dots,\textsf{l}_i=\textsf{v}_i,\dots,\textsf{l}_n=\textsf{v}_n\}.\textsf{l}_i\rarr\textsf{v}_i \quad i \in1..n
$$

## 演習 11.8.2. [\star\star\star$] 

### (1) 型付け規則

パターン束縛に関して次の規則を加える。
$$
\frac{
	\Gamma \vdash {\sf t_1:T_1}
	\qquad
	\Gamma,{\sf p:T_1} \vdash {\sf t_2:T_2}
}{
    \Gamma \vdash {\sf let\ p=t_1\ in\ t_2:T_2}
} \quad (\textrm{T-Ptn})
$$
ここでパターンに型が付くことが前提になっている ($\sf p:T_1$) が、パターンは変数かレコードであるため、それらの型付け規則はT-VarおよびT-Rcdで賄うことができる。 

### (2) 進行、保存則の証明概略

- 進行（項がwell-typedならば値or評価可能）：型付け導出に関する帰納法
  - T-Let: 帰結部は $\Gamma \vdash {\sf let\ x=t_1\ in\ t_2:T_2}$
    - $\sf t_2$ に依らず以下の場合分けで評価が進行する
      - $\sf t_1$ が値→E-LetV
      - $\sf t_1$ が評価可能→E-Let
  - T-Rcd: 帰結部は $\Gamma \vdash \{\textsf{l}_i=\textsf{t}_i\ ^{i\in1..n}\}: \{\textsf{l}_i:\textsf{T}_i\ ^{i\in1..n}\}$
    - $\forall i \in 1..n. \textsf{t}_i$ が値→レコード型への標準形補題
      - $\textsf{v} : \{\textsf{l}_i:\textsf{T}_i\ ^{i\in1..n}\} \longrightarrow \textsf{v} = \{\textsf{l}_i:\textsf{v}_i\ ^{i\in1..n}\}$ (証明略)
    - otherwise→ $\exists i \in 1..n. \textsf{t}_i$ が評価可能→E-Rcd
  - T-Proj: 帰結部は $\Gamma \vdash \textsf{t}_1.\textsf{l}_j:\textsf{T}_j$
    - $\textsf{t}_1 = \{\textsf{l}_i:\textsf{v}_i\ ^{i\in1..n}\}$ （フィールドがすべて値）→E-ProjRcd
    - otherwise→E-Proj 
  - T-Ptn ((1) で定義): 帰結部は $\Gamma \vdash {\sf let\ p=t_1\ in\ t_2:T_2}$
    - T-Let と同様
- 保存（well-typedな項を評価して得た項はwell-typed）
  - T-Let: 帰結部は $\Gamma \vdash {\sf let\ x=t_1\ in\ t_2:T_2}$
    - $\Gamma \vdash {\sf t_1:T_1}$, $\Gamma, {\sf x:T_1}\vdash {\sf t_2:T_2}$ を帰結とする部分導出がある
    - 適用可能な評価規則は E-LetV, E-Let
      - E-LetV: $\sf t_1 = v_1 \quad t' = [x\mapsto v_1]t_2$ → 代入補題
      - E-Let: $\sf t_1 \rarr t_1' \quad t' = let\ x=t_1'\ in\ t_2$ → T-Let
  - T-Rcd: 帰結部は $\Gamma \vdash \{\textsf{l}_i=\textsf{t}_i\ ^{i\in1..n}\}: \{\textsf{l}_i:\textsf{T}_i\ ^{i\in1..n}\}$
    - 各 $i$ に対して $\Gamma \vdash \textsf{t}_i:\textsf{T}_i$ を帰結とする部分導出がある
    - 適用可能な評価規則は E-Rcd → T-Rcd
  - T-Proj: 帰結部は $\Gamma \vdash \textsf{t}_1.\textsf{l}_j:\textsf{T}_j$
    - $\Gamma \vdash \textsf{t}_1 : \{\textsf{l}_i:\textsf{T}_i\ ^{i\in1..n}\}$ を帰結とする部分導出がある
    - 適用可能な評価規則は E-ProjRcd, E-Proj
      - E-ProjRcd → 各 $i$ に対して $\textsf{l}_i:\textsf{T}_i$ が部分導出で得られている
      - E-Proj → T-Proj
  - T-Ptn ((1) で定義): 帰結部は $\Gamma \vdash {\sf let\ p=t_1\ in\ t_2:T_2}$
    - T-Let と同様

> (1) パターン型付け規則を導入し、 $\Gamma$ に加えて $\Delta$ を環境として持つ。
> レコードパターン型付けを行う際、マッチ対象の値が実際に持つフィールドよりも少ないフィールドにしか言及しないように改造すると、レコード射影はletへの糖衣構文になる
>
> (2) パターン型付け関係と実行時のマッチ演算を対応させる補題が必要
>
> - $\Gamma \vdash \sigma \vDash \Delta$ は、 $\Delta$ の定義域である各パターンについて、代入 $\sigma$ を実行しても $\Gamma$ 上の型はそのままである…みたいな
> - 項もパターンも同じ型で、環境 $\Delta$ に含まれるなら、マッチは代入に評価され、 $\Gamma \vdash \sigma \vDash \Delta$ が成り立つという補題
>   - さらに、代入補題化： $\Gamma, \Delta \vdash {\sf t : T}$ かつ $\Gamma \vdash {\sf \sigma \vDash \Delta}$ ならば $\Delta \vdash \sigma {\sf t:T}$