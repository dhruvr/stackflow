
# Get selected value in dropdown list using JavaScript?

## Question
        
How do I get the selected value from a dropdown list using JavaScript?

I tried the methods below but they all return the selected index instead of the value:

    var as = document.form1.ddlViewBy.value;
    var e = document.getElementById("ddlViewBy");
    var strUser = e.options[e.selectedIndex].value;
    var value = document.getElementById("ddlViewBy").value;

## Answer
        
If you have a select element that looks like this:

    <select id="ddlViewBy">
      <option value="1">test1</option>
      <option value="2" selected="selected">test2</option>
      <option value="3">test3</option>
    </select>
    

Running this code:

    var e = document.getElementById("ddlViewBy");
    var strUser = e.options[e.selectedIndex].value;
    

Would make `strUser` be `2`. If what you actually want is `test2`, then do this:

    var e = document.getElementById("ddlViewBy");
    var strUser = e.options[e.selectedIndex].text;
    

Which would make `strUser` be `test2`
