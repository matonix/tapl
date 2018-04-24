// 5.2
const tru = t => f => t
const fls = t => f => f
const test = l => m => n => l(m)(n)

// test(tru)("a")("b")
// "a"

const and = b => c => b(c)(fls)

// 5.2.1
const or = b => c => b(tru)(c)
const not = b => b(fls)(tru)

// pair
const pair = f => s => b => b(f)(s)
const fst = p => p(tru)
const snd = p => p(fls)

// church number
const c0 = s => z => z
const c1 = s => z => s(z)
const c2 = s => z => s(s(z))
const c3 = s => z => s(s(s(z)))

const scc = n => s => z => s(n(s)(z))

// 5.2.2
const scc_ = n => s => z => n(s)(s(z))

const plus = m => n => s => z => m(s)(n(s)(z))

const times = m => n => m(plus(n))(c0)

// 5.2.3
const times_ = m => n => s => z => m(n(s))(z)

// 5.2.4
const power = m => n => n(times(m))(c1)
const power_ = m => n => s => z => (n(m))(s)(z)

const iszro = m => m(x => fls)(tru)

const zz = pair(c0)(c0)
const ss = p => pair(snd(p))(plus(c1)(snd(p)))
const prd = m => fst(m(ss)(zz))

// 5.2.5
const subtract = m => n => n(prd)(m)

// 5.2.6 O(n)

// 5.2.7
const equal = m => n => and(iszro(m(prd)(n)))(iszro(n(prd)(m)))

// 5.2.8
const nil = c => n => n
const cons = h => t => c => n => c(h)(t(c)(n))
const head = l => l(h => t => h)(fls)
const tail = l => fst(l(x => p => pair(snd(p))(cons(x)(snd(p))))(pair(nil)(nil)))
const isnil = l => l(h => t => fls)(tru)

const nil_ = pair(nil)(nil)
const cons_ = h => t => pair(fls)(pair(h)(t))
const head_ = z => fst(snd(z))
const tail_ = z => snd(snd(z))
const isnil_ = fst

const xyz = c => n => c('x')(c('y')(c('z')(n)))

// lambdaNB
const realbool = b => b(true)(false)
const churchbool = b => b ? tru : fls
const realeq = m => n => (equal(m)(n))(true)(false)
const realnat = m => m(x => 1 + x)(0)

// recursive
const omega = x => x(x)(x => x(x))
const fix = f => (x => f(y => x(x)(y)))(x => f(y => x(x)(y)))

// 5.2.9
const factorial = fix(f => n => test(realeq(n)(c0))(x => c1)(x => times(n)(f(prd(n))))(c0))

// 5.2.10
const churchnat = fix(f => m => m == 0 ? c0 : scc(f(m-1)))

// 5.2.11
const sumlist = fix(f => l => test(isnil(l))(x => c0)(x => plus(head(l))(f(tail(l))))(c0))

// realnat(power(churchnat(10))(churchnat(5)))
// 100000
