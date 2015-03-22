var milkcocoa = new MilkCocoa("https://io-bi74939df.mlkcca.com");
var ds = milkcocoa.dataStore("doubutsuShogi");

function clickEvent(){
  sendText(text);
}

function sendText(text){
  ds.push({message : text},function(data){
    console.log("送信完了!");
  });
}
