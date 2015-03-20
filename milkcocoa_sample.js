var milkcocoa = new MilkCocoa("https://io-bi74939df.mlkcca.com");
var ds = milkcocoa.dataStore("doubutsuShogi");
// var textArea, board;
// window.onload = function(){
//   textArea = document.getElementById("msg");
//   board = document.getElementById("board");
// }

function clickEvent(){
//  var text = textArea.value;
  sendText(text);
}

function sendText(text){
  ds.push({message : text},function(data){
    console.log("送信完了!");
//    textArea.value = "";
  });
}
