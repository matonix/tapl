# 第 8 章 型付き算術式

## 8.1 型

## 8.2 型付け関係

定義 8.2.1. 

- 型付け関係 typing relation
- 型付け可能 typable
- 正しく型付けされている well typed

補題 8.2.2. [型付け関係の逆転]

- 別名: 逆転補題 および 生成補題
- 項の形からその直接の部分項の型が決まる

### 演習 8.2.3. $[\star \nrightarrow]$

> 言い換え: 任意の項 t について、t  が well typed ならば t の任意の部分項は well typed である。

[証明] 項 t 上の帰納法による。

t が true, false, 0 のとき直ちに成り立つ。

t が if t1 then t2 else t3 の形のとき、 t が well typed ならば補題 8.2.2. (3) より t1, t2, t3 も well typed であり、帰納法の仮定より t の任意の部分項は well typed である。

以下、succ t1, pred t1, iszero t1 の場合も同様。

---

定理 8.2.4. [型の一意性]

## 8.3 安全性 = 進行 + 保存

補題 8.3.1. [標準形]

- 型が決まれば値のケースが絞られる

定理 8.3.2. [進行]

- t が well typed ならば t は値であるか評価が進行する
- [証明概略] t : T の導出に関する帰納法。型付け規則（T-Xxx）によって場合分け。

定理 8.3.3. [保存]

- t が well typed で評価できるならば、評価後も well typed である
- [証明概略] t : T の導出に関する帰納法。型付け規則（T-Xxx）によって場合分け。

### 演習 8.3.4. $[\star\star \nrightarrow]$

- [参照] 評価導出: B に関しては p.25, NB に関しては p.30

[証明] t の評価導出による帰納法による。 帰納法の各ステップにおいて、すべての部分導出に関して所望の性質が成り立つと仮定する。 そして、導出の最後の規則についての場合分けにより証明を進める。

- E-True の場合 t = true
  - 導出の最後の規則がE-Trueならば、t が定数 true である。これに該当する型付け規則は T-True のみであり、T は Bool となる。しかし、t は値なのでどんな t' に対しても t → t' とならず、定理の要求は自明に満たされる。
  - E-False も同様。
- E-IfTrue の場合 t = if true then t2 else t3
  - 導出の最後の規則がE-IfTrueならば、その規則の形から、 t はある t2, t3 に対して if true then t2 else t3 という形でなければならない。また、結果の項 t' は t2 となることがわかる。このとき、左側に if が現れる型付け規則は T-If のみであり、生成補題より t : T ならば t2 : T, t3 : T となるため、所要の結果となる。
  - E-IfFalse も同様。
- E-If の場合 t1 → t1', t = if t1 then t2 else t3
  - 導出の最後の規則がE-Ifならば、その規則の形から、 t はある t1, t2, t3 に対して if t1 then t2 else t3 という形でなければならない。このとき、左側に if が現れる型付け規則は T-If のみであり、生成補題より t : T ならば t1 : Bool, t2 : T, t3 : T となる。このとき E-If の仮定より t1 → t1' であるため、t1 : Bool かつ t1 → t1' より t1' : Bool を得る。この事実と t2 : T, t3 : T を合わせると T-If が適用でき、 if t1' then t2 else t3 : T となり、つまり t' : T が成り立つ。
- E-PredZero の場合 t = pred 0
  - （略記） 左側が pred となる型付け規則は T-Pred で、t : Nat ならば t1 : Nat となる。仮定より t1 = 0 であり、 T-Zero より t1 : Nat となるため、所要の結果となる。
  - E-IszeroZero も似たような論調になると思われる。
- E-PredSucc の場合 t = pred (succ nv1)
  - （略記） 左側が pred となる型付け規則は T-Pred で t : Nat ならば t1 : Nat となる。仮定より t1 = succ nv1 であり、T-Succ の帰納法の仮定より nv1 : Nat (= t') となるため、所要の結果となる。
  - E-IszeroSucc も似たような論調になると思われる。
- E-Succ の場合 t2 → t2', t = succ t2 （記号が T-Succ とかぶるので t2 にした）
  - （略記）左側が succ となる型付け規則は T-Succ で t : Nat ならば t1 : Nat となる。仮定より t1 = t2 かつ t2 → t2' なので t2' : Nat である。このとき T-Succ が適用でき、 succ t2' : Nat (= t') となるため、所望の結果となる。
  - E-Pred 同様。

### 演習 8.3.5. $[\star]$

> このことを達成する = 進行と保存を満たす

達成できない。 反例は pred 0 である。

pred 0 に対して型付け導出を行うと、T-Pred → T-Zero の順で導出でき、よって pred 0 は well typed である。ところが、pred 0 は値でなく、かつ評価を進行させる評価規則が存在しない（E-PredZeroが存在しないので）ため、進行定理を満たさない。

### 演習 8.3.6. $[推奨, \star\star]$

>  t → t' かつ t' : T ならば t : T が成り立つか？

成り立たない。反例は if true then 0 else true である。

if true then 0 else true に対して E-IfTrue による評価が可能であり、結果は 0 となる。 0 に対しては T-Zero が、 true に対しては T-True によって型付けでき、 0 : Nat (= t'), true : Bool となる。このとき、if true then 0 else true (= t) に適用できる型付け規則は存在せず、ゆえに成り立たない。

### 演習 8.3.7. $[推奨, \star\star]$

大ステップ評価に対して進行と保存が成立することである。すなわち、以下が成立すればよい。

- [進行] t が well typed と仮定すると、 t は値であるか、ある t' が存在して t ↑ t'
- [保存] t : T かつ t ↑ t' ならば t' : T

### 演習 8.3.8. $[推奨, \star\star]$

wrongを除いた評価に対して進行と保存が成立することである。すなわち、以下が成立すればよい。

- [進行] t が well typed と仮定すると、 t は値であるか、ある wrong ではない t' が存在して t → t'
- [保存] ある wrong ではない t' が存在して t : T かつ t → t' ならば t' : T

