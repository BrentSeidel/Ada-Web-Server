//
// Simple function to put some output into the "demo" element.
//
function test()
{
  var text = "";
  var x = 0;

  for (x = 0; x < 10; x++)
  {
    text += "Test " + x + "<br>";
  }
  document.getElementById("demo").innerHTML = text;
}
//
// Get numbers from two fields, add them together and put the result in another
// field and the "demo" element.
//
function calculate()
{
  var num1 = document.getElementById("num1").value * 1;
  var num2 = document.getElementById("num2").value * 1;

  document.getElementById("num3").value = num1 + num2;
  document.getElementById("demo").innerHTML = "Calculating " + (num1 + num2);
}
//
// Requests a value from the server using AJAX.
//
function loadCounter()
{
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
    displayCounter(this);
    }
  };
  xhttp.open("GET", "/xml/Counter", true);
  xhttp.send();
}
//
// Displays the value requested in the previous function in the "counter" element.
//
function displayCounter(xml)
{
  var xmlDoc = xml.responseXML;
  var x = xmlDoc.getElementsByTagName("counter")[0].childNodes[0].nodeValue;
  document.getElementById("count").innerHTML = x + " transactions have been processed";
}
//
// Global values for some timer related functions.
//
var therm_value;
var therm_timer;
//
// Initializes the value and starts a timer to count up.
//
function animate()
{
  therm_value = 0;
  therm_timer = window.setInterval(update_therm_up, 250);
}
//
// Counts up and updates a couple images on each count.
//
function update_therm_up()
{
  var el1 = document.getElementById("temp1");
  var el2 = document.getElementById("temp2");
  var el3 = document.getElementById("dial1");
  el1.src = "/Thermometer?min=100&max=350&value=" + (350 - therm_value);
  el2.src = "/Thermometer?min=0&max=250&value=" + therm_value;
  el3.src = "/Dial?min=100&max=200&value=" + therm_value;
  therm_value++;
  if (therm_value > 250)
  {
    window.clearInterval(therm_timer);
  therm_timer = window.setInterval(update_therm_down, 250);
  }
}
//
// Counts down and updates a couple images on each count.
//
function update_therm_down()
{
  var el1 = document.getElementById("temp1");
  var el2 = document.getElementById("temp2");
  var el3 = document.getElementById("dial1");
  el1.src = "/Thermometer?min=100&max=350&value=" + (350 - therm_value);
  el2.src = "/Thermometer?min=0&max=250&value=" + therm_value;
  el3.src = "/Dial?min=100&max=200&value=" + therm_value;
  therm_value--;
  if (therm_value < 0)
  {
    window.clearInterval(therm_timer);
  }
}
//
// Set the value of thermometer 1
//
function setT1()
{
  var el = document.getElementById("temp1");
  el.src = "/Thermometer?min=100&max=350&value=" + document.getElementById("t1").value;
}
//
// Set the value of thermometer 2
//
function setT2()
{
  var el = document.getElementById("temp2");
  el.src = "/Thermometer?min=0&max=250&value=" + document.getElementById("t2").value;
}
//
// Set the value of dial 1
//
function setD1()
{
  var el = document.getElementById("dial1");
  el.src = "/Dial?min=100&max=200&value=" + document.getElementById("d1").value;
}

