# 第 11 章 単純な拡張

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

