var Web3 = require("web3")
var BigNumber = require("bignumber")
var winston = require("winston")

var WS_SERVER = "http://localhost:8545"
var web3 = new Web3(Web3.givenProvider || WS_SERVER);

// var hash = web3.eth.accounts.hashMessage("Hello World")
// winston.log('info', 'Hash:' + hash)
var maxBlock = 10; //web3.eth.getBlockNumber()
for (var i = 0; i < maxBlock; i++) {
  web3.eth.getBlock(i).then(res => {
    winston.log('info', 'Fetching block:' + i)
    winston.log('info', 'Block:' + JSON.stringify(res))
  })
}