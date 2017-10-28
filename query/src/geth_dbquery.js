
const Trie = require('merkle-patricia-tree');
const rlp = require('rlp');
const levelup = require('levelup');

const DB_PATH = '/Users/hisham/Library/Ethereum/geth/chaindata';
//the genesis state root
const ROOT = '0xd7f8974fb5ac78d9ac099b9ad5018bedc2ce0a72dad1827a1709da30580f0544';

var db = levelup(DB_PATH);
var trie = new Trie(db, ROOT);

//get a read stream object
var stream = trie.createReadStream();

stream.on('data', function (data) {
  console.log('key:' + data.key.toString('hex'));
  // accouts are rlp encoded
  var decodedVal = rlp.decode(data.value);

  // the decoded value is balance, none, state hash (storage hash), and code hash (byte codes).
  // code hash and state hash are for contracts.
  console.log(decodedVal[0]);
});

stream.on('end', function (val) {
  console.log('done reading!');
});
