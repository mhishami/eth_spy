#!/bin/sh
#
curl http://localhost:8983/solr/ethereum/config/params -H 'Content-type:application/json' -d '{
  "set": {
    "blockchain": {
      "split": "/transactions",
      "f": [
        "id:/transactions/hash",
        "author:/author",
        "difficulty:/difficulty",
        "extraData:/extraData",
        "gasLimit:/gasLimit",
        "gasUsed:/gasUsed",
        "currentBlockHash:/hash",
        "logsBloom:/logsBloom",
        "miner:/miner",
        "mixHash:/mixHash",
        "nonce:/nonce",
        "number:/number",
        "parentHash:/parentHash",
        "receiptsRoot:/receiptsRoot",
        "sealFields:/sealFields",
        "sha3Uncles:/sha3Uncles",
        "size:/size",
        "stateRoot:/stateRoot",
        "timestamp:/timestamp",
        "blockHash:/transactions/blockHash",
        "blockNumber:/transactions/blockNumber",
        "condition:/transactions/condition",
        "creates:/transactions/creates",
        "from:/transactions/from",
        "gas:/transactions/gas",
        "gasPrice:/transactions/gasPrice",
        "txHash:/transactions/hash",
        "txInput:/transactions/input",
        "networkId:/transactions/networkId",
        "txNonce:/transactions/nonce",
        "publicKey:/transactions/publicKey",
        "r:/transactions/r",
        "s:/transactions/s",
        "v:/transactions/v",
        "raw:/transactions/raw",
        "standardV:/transactions/standardV",
        "to:/transactions/to",
        "transactionIndex:/transactions/transactionIndex",
        "value:/transactions/value",
        "totalDifficulty:/totalDifficulty",
        "transactionsRoot:/transactionsRoot",
        "uncles:/uncles"
      ]
    }
  }
}'
