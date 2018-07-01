# 第 6 章 項の型無し表現

## 演習 6.1.1. $[\star]$

> ${\sf c_0 = \lambda s. \lambda z. z;}$

${\sf c_0 = \lambda . \lambda . 0;}$

> ${\sf c_2 = \lambda s. \lambda z. s\ (s\ z);}$

${\sf c_0 = \lambda . \lambda . 1\ (1\ 0);}$

> ${\sf plus = \lambda m. \lambda n. \lambda s. \lambda z. m\ s\ (n\ s\ z);}$

${\sf plus = \lambda . \lambda . \lambda . \lambda . 3\ 1\ (2\ 1\ 0);}$

> ${\sf fix = \lambda f. (\lambda x. f\ (\lambda y. (x\ x)\ y)) (\lambda x. f\ (\lambda y. (x\ x)\ y));}$

${\sf fix = \lambda . (\lambda . 1\ (\lambda . (1\ 1)\ 0)) (\lambda . 1\ (\lambda y. (1\ 1)\ 0));}$

> ${\sf foo = (\lambda x. (\lambda x. x)) (\lambda x. x);}$

${\sf foo = (\lambda . (\lambda . 0)) (\lambda . 0);}$

- 自己採点: ok

## 演習 6.1.4. $[\star\star\star \nrightarrow]$

#### 再掲: 定義 6.1.2.

> $\mathcal{T}$ を、以下の条件を満たす集合の最小の族 $\{\mathcal{T}_0, \mathcal{T}_1, \mathcal{T}_2, ...\}$ とする
> (1) $ 0 \le \textsf{k} < n$ ならば $ \textsf{k} \in \mathcal{T}_n$
> (2) $ \textsf{t}_1 \in \mathcal{T}_n$ かつ $n > 0$ ならば $\lambda. \textsf{t}_1 \in \mathcal{T}_{n-1}$
> (3) $\textsf{t}_1 \in \mathcal{T}_n$ かつ $\textsf{t}_2 \in \mathcal{T}_n$ ならば ${\sf (t_1\ t_2)} \in \mathcal{T}_n$

### 構成

$n$ 項の集合 $\mathcal{S} = \{\mathcal{S}_0, \mathcal{S}_1, \mathcal{S}_2, ...\}$ を以下のように構成する。

$$
\mathcal{S}_n = \bigcup_i \mathcal{S}_n^i \qquad
\mathcal{S}_n^0 = \phi \qquad
\begin{eqnarray}
\mathcal{S}_n^{i+1} & = &      & \{ 0, \dots, n-1 \} & \qquad \cdots (1')\\
                    &  & \cup & \{ \lambda. \textsf{t}_1\ |\ \textsf{t}_1 \in \mathcal{S}_{n+1}^i \}  &\qquad \cdots (2')\\
                    &  & \cup & \{ \textsf{t}_1\ \textsf{t}_2\ |\ \textsf{t}_1, \textsf{t}_2 \in \mathcal{S}_n^i \}  &\qquad \cdots (3')\\
\end{eqnarray}
$$

### 証明方針

- (a) $\mathcal{S}$ が $\mathcal{T}$ の条件（定義 6.1.2.）を満たすことを示す
- (b) $\mathcal{T}_n$ を満たす任意の集合が $\mathcal{S}_n$ を部分集合として持つ
  （すなわち、$\mathcal{S}_n$ が $\mathcal{T}_n$ の条件を満たす最小の集合である）ことを示す

### 証明

- (a)
  - (1) (1') より、 $\mathcal{S}_n^1 = \{ 0, \dots, n-1 \} \sube \mathcal{S}_n$ なので、  $ 0 \le \textsf{k} < n$ ならば $ \textsf{k} \in \mathcal{S}_n \in \mathcal{S}$
  - (2)  $ \textsf{t}_1 \in \mathcal{S}_n$ かつ $n > 0$ ならば、ある $i$ について $ \textsf{t}_1 \in \mathcal{S}_n^i$ とならねばならない。
    このとき (2') より $\lambda. \textsf{t}_1 \in \mathcal{S}_{n-1}^{i+1} \sube \mathcal{S}_{n-1} \in \mathcal{S}$  
  - (3) $\textsf{t}_1 \in \mathcal{S}_n$ かつ $\textsf{t}_2 \in \mathcal{S}_n$ ならば、ある $i$ について $ \textsf{t}_1, \textsf{t}_2 \in \mathcal{S}_n^i$ とならねばならない。
    このとき (3') より ${\sf (t_1\ t_2)} \in \mathcal{S}_n^{i+1} \sube \mathcal{S}_n \in \mathcal{S}$
- (b)
  - 方針：ある $\mathcal{S}_n'$ が定義 6.1.2. を満たすと仮定する。 $i$ に関する完全帰納法を用いて、すべての $i$ について $\mathcal{S}_n^i \sube \mathcal{S}_n'$ を示す。これから $\mathcal{S}_n \sube \mathcal{S}_n'$ が得られるのは明らか。
  - すべての $j<i$ について、 $\mathcal{S}_n^j \sube \mathcal{S}_n'$ を仮定し、 $\mathcal{S}_n^i \sube \mathcal{S}_n'$ を示す。
    - $i = 0$ のとき、 $\mathcal{S}_n^i = \phi \sube \mathcal{S}_n'$ 
    - $i>0$ のとき、$i = j + 1$ なる $j$ が存在して、 $\textsf{t} \in \mathcal{S}_n^{j+1}$ とする。
      - $\textsf{t}$ が定数ならば、(1) より $\textsf{t} \in \mathcal{S}_n'$ 
      - $\textsf{t}$ がある $\textsf{t}_1 \in \mathcal{S}_{n+1}^j$ に対して $\lambda. \textsf{t}_1$ の形ならば、 $\textsf{t}_1 \in \mathcal{S}_{n+1}'$ であり、かつ、 $n+1>0$ なので (2) より $\lambda. \textsf{t}_1 \in \mathcal{S}_n'$
      - $\textsf{t}$ がある $\textsf{t}_1, \textsf{t}_2 \in \mathcal{S}_n^j$ に対して $\textsf{t}_1\ \textsf{t}_2$ の形ならば、 $\textsf{t}_1, \textsf{t}_2 \in \mathcal{S}_n'$ なので、 (3) より $(\textsf{t}_1\ \textsf{t}_2) \in \mathcal{S}_n'$
  - よって、すべての $i$ について $\mathcal{S}_n^i \sube \mathcal{S}_n'$ を証明した。 $\mathcal{S}_n$ は $\mathcal{S}_n^i$ の和集合なので、 $\mathcal{S}_n \sube \mathcal{S}_n'$ が得られる。

## 演習 6.1.5. $[推奨, \star\star\star]$

### (1) $removenames_\Gamma(\textsf{t})$を定義せよ

$$
removenames_\Gamma(\textsf{t}) = rem_\Gamma(\textsf{t}, 0, \phi)\\
where\quad
\begin{eqnarray}
rem_\Gamma(\textsf{x}, d, D) &=& 
\begin{cases}
d+\Gamma(\textsf{x}) & \textsf{x} \in dom(\Gamma)\\
d-1-D(\textsf{x}) & otherwise
\end{cases}\\
rem_\Gamma(\lambda\textsf{x}. \textsf{t}_1, d, D) &=& \lambda.rem_\Gamma(\textsf{t}_1, d+1, D\cup\{\textsf{x} \mapsto d\})\\
rem_\Gamma(\textsf{t}_1\ \textsf{t}_2, d, D) &=& rem_\Gamma(\textsf{t}_1, d, D)\ rem_\Gamma(\textsf{t}_2, d, D)\\
\end{eqnarray}
$$

### (2) $restorenames_\Gamma(\textsf{t})$を定義せよ

- あるマッピングの集合 $\Alpha$ に対し $\Alpha^{-1}$ のように書いたとき、マッピングの方向が逆になった集合を表すとする
  - 例: $\{x \mapsto 4\}^{-1} = \{4 \mapsto x\}$

$$
restorenames_\Gamma(\textsf{t}) = res_\Gamma(\textsf{t}, 0, \phi)\\
where\quad
\begin{eqnarray}
res_\Gamma(n, d, D) &=& 
\begin{cases}
\Gamma^{-1}(n-d) & n-d \in dom(\Gamma^{-1})\\
D^{-1}(d-1-n) & otherwise
\end{cases}\\
res_\Gamma(\lambda. \textsf{t}_1, d, D) &=& \lambda \textsf{x}_{fresh}. res_\Gamma(\textsf{t}_1, d+1, D\cup\{\textsf{x}_{fresh} \mapsto d\})\\
& & where\quad \textsf{x}_{fresh} = fresh(\mathcal{V}\setminus(\Gamma \cup D))\\
& & where\quad fresh(X) = X^{-1}(\min dom(X^{-1}))\\
res_\Gamma(\textsf{t}_1\ \textsf{t}_2, d, D) &=& res_\Gamma(\textsf{t}_1, d, D)\ res_\Gamma(\textsf{t}_2, d, D)\\
\end{eqnarray}\\
$$

- 自己採点: わかりません（最も右にある $\textsf{x}$ のインデックスを求めるのに、再帰的定義だと現在の深さ $d$ とラムダ抽象  $\lambda \textsf{x}.$ に付随する変項 $\textsf{x}$ が出現したときの深さを保持する集合 $D$ が必要なんじゃない？というアイディアで書きました）

## 演習 6.2.2. $[\star]$

省略

## 演習 6.2.3. $[\star\star \nrightarrow]$

- 任意の $\textsf{t}, d, c$ に対して $\begin{cases} \uparrow^d_c(\textsf{t}) \leq n+d\ 項\\ \uparrow^d_c(\textsf{t}) \geq 0\ 項 \end{cases}$ であることを示したい
- とりあえず境界部分のケースを考える
  - $n+d$ 項となる場合: $\textsf{t} = 0\ 1\ \dots(n-d-1)\ \underbrace{(\lambda. \dots (\lambda.}_{d-1}\ (n-d)\ \dots\ (n-1)\underbrace{)\dots)}_{d-1}\ (n-d)\ \dots\  (n-1)\  \quad (c=n-d-1, 0\leq d\leq n-1)$
    - 実際にシフトすると: $\uparrow^d_{n-d-1}(\textsf{t}) =  0\ 1\ \dots(n-d-1)\ \underbrace{(\lambda. \dots (\lambda.}_{d-1}\ (n-d)\ \dots\ (n-1)\underbrace{)\dots)}_{d-1}\ n\ \dots\  (n-1+d)$
  - $0$ 項となる場合: $\textsf{t} = 0$
    - $0$ 項であり $n$ 項でもあるはず…
- $n+d+1$ 項以上は作り得ないことを示すには $n+d$ 以上のインデックスを作り得ないことを示せばよい 
  - 任意の項で $\uparrow^d_c$ なるシフトで内部的に $d+1$ 以上シフトされるインデックスが存在し得ないことを示す
  - シフトの定義から自明と言える？

## 演習 6.2.5. $[\star]$

省略

## 演習 6.2.6. $[\star\star \nrightarrow]$

[TODO]

## 演習 6.2.7. $[\star \nrightarrow]$

省略（ちゃんと書きましたよ…？）

## 演習 6.2.8. $[推奨, \star\star\star]$

### (1) どのような定理が証明される必要があるか

通常の項に対して代入を行ってから名前を除去したときと、名前を除去してから代入を行ったときで同じ項が得られる

### (2) 証明せよ

[TODO]

## 演習 6.3.1 $[\star]$

- $\textsf{t}_{12}$ と $\textsf{v}_2$ が負のインデックスを含まない限り、負のインデックスを含む項が作られるおそれはない。

E-AppAbs では、 $\textsf{t}_{12}$ 中の 0 の出現が全て $\uparrow^1(\textsf{v}_2)$ に置き換わる。そのため、 $\uparrow^1(\textsf{v}_2)$ の自由変数が全て 1 以上ならば、$\textsf{t}_{12}$ 中の変数はすべて 1 以上である。

また、 $\textsf{v}_2$ 中の 0 以上の自由変数は $\uparrow^1(\textsf{v}_2)$ においてすべて 1 以上となっている。

以上から、$[0 \mapsto \uparrow^1(\textsf{v}_2)]\textsf{t}_{12}$ の自由変数はすべて 1 以上であり、E-AppAbs によって負のインデックスを含む項は出現しない。

## 演習 6.3.2 $[\star\star\star]$

De Bruijn レベルを、通常の項 $\textsf{t}$ に対する名無し項構成関数 $removenames'_\Gamma(\textsf{t})$ を構成することによって定義する。
$$
removenames'_\Gamma(\textsf{t}) = rem'_\Gamma(\textsf{t}, 0, \phi)\\
where\quad
\begin{eqnarray}
rem'_\Gamma(\textsf{x}, d, D) &=& 
\begin{cases}
d+\Gamma(\textsf{x}) & \textsf{x} \in dom(\Gamma)\\
D(\textsf{x}) & otherwise
\end{cases}\\
rem'_\Gamma(\lambda\textsf{x}. \textsf{t}_1, d, D) &=& \lambda.rem'_\Gamma(\textsf{t}_1, d+1, D\cup\{\textsf{x} \mapsto d\})\\
rem'_\Gamma(\textsf{t}_1\ \textsf{t}_2, d, D) &=& rem'_\Gamma(\textsf{t}_1, d, D)\ rem'_\Gamma(\textsf{t}_2, d, D)\\
\end{eqnarray}
$$
変化があるのは実質 $\textsf{x}$ がマッチした場合の第2項だけであり、$d-1-D(\textsf{x})$ が $D(\textsf{x})$ に変化している。

つまり、束縛変数のレベルはそのままラムダ項として出現したときのラムダの深さとなる。

同様に名無し項 $\textsf{t}$ に対する通常の項構成関数 $restorenames'_\Gamma(\textsf{t})$ を構成する。

[TODO]

演習 6.1.5. と同様、 2つの関数が逆写像（のような）性質を持つことができれば、通常の項を経由してインデックスとレベルは相互に復元できるため、インデックスとレベルは同型となる。

2つの関数が逆写像（のような）性質を持つことに関する証明は [TODO]

- ここまで書いておいて思ったが、逆写像性を言えたからといって変換の一意性があるわけではないから、通常の項を経由する方法はうまく行かないかも…