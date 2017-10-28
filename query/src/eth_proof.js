const Web3 = require('web3')
const EP  = require('eth-proof')

var chainDataPath = '/Users/hisham/Library/Ethereum/geth/chaindata'
var eP = new EP(
  new Web3.providers.HttpProvider("https://gmainnet.infura.io"),
  'a61b780b1c2f6a79d052e4b58234dc126fd7fdc9338705983d6068965ba8384b',
  chainDataPath //can be omitted if you only need TXs and receipts
)

var txHash = '0xb53f752216120e8cbe18783f41c6d960254ad59fac16229d4eaec5f7591319de'
eP.getTransactionProof(txHash).then((result)=>{
  // console.log(result) // I now have a proof object

  // I can now verify the proof against a blockhash I trust.
  var myTrustedBlockHash = Buffer.from('f82990de9b368d810ce4b858c45717737245aa965771565f8a41df4c75acc171','hex')
  var verified = EP.transaction(result.path, result.value, result.parentNodes, result.header, myTrustedBlockHash)
  console.log(verified) // true
}).catch((e)=>{console.log(e)})
