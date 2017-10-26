var solr = require('solr-client');

var options = {
  path: "/solr/ethereum"
}
var client = solr.createClient(options);

// our query
var query = client.createQuery()
  .q('value:[100 TO *]')
  .fl('from, to, value')
  .rows(10);
client.search(query, function(err,obj) {
  if (err) {
    console.log(err);
  } else {
    var resp = obj.response.docs;
    for (r of resp) {
      console.log("resp", r);
    }
  }
});