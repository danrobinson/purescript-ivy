const parser = require("./output/Parse/index.js")

const con = parser.parseContract(`contract VaultSpend(hotKey: PublicKey, coldKey: PublicKey, delay: Duration, val: Value) {
  clause cancel(sig: Signature) {
    verify checkSig(coldKey, sig)
    unlock val
  }
  clause complete(sig: Signature) {
    verify older(delay)
    verify checkSig(hotKey, sig)
    unlock val
  }
}`)

for (const prop in con) {
  console.log(prop)
  console.log(con[prop])
}
