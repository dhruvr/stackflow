
# How to pass JavaScript variables to PHP?

## Question
        
I want to pass JavaScript variables to PHP using a hidden input in a form.

But I can't get the value of `$_POST['hidden1']` into `$salarieid`. Is there something wrong?

Here is the code:

    <script type="text/javascript">
    // view which the user has chosen
    function func_load3(name){
        var oForm = document.forms["myform"];
        var oSelectBox = oForm.select3;
        var iChoice = oSelectBox.selectedIndex;
        //alert("you have choosen: " + oSelectBox.options[iChoice].text );
        //document.write(oSelectBox.options[iChoice].text);
        var sa = oSelectBox.options[iChoice].text;
        document.getElementById("hidden1").value = sa;
    }
    </script>
    
    <form name="myform" action="<?php echo $_SERVER['$PHP_SELF']; ?>" method="POST">
            <input type="hidden" name="hidden1" id="hidden1"  />
    </form>
    
    <?php
       $salarieid = $_POST['hidden1'];
       $query = "select * from salarie where salarieid = ".$salarieid;
       echo $query;
       $result = mysql_query($query);
    ?>
    
    <table>
       code for display the query result. 
    </table>

## Answer
        
You cannot pass variable values from the current page javascript to the current page PHP code... PHP code runs at the server side and it doesn't know anything about what is going on on the client side.

You need to pass variables to PHP code from html-form using another mechanism, such as submitting form on GET or POST methods.

    <DOCTYPE html>
    <html>
      <head>
        <title>My Test Form</title>
      </head>
    
      <body>
        <form method="POST">
          <p>Please, choose the salary id to proceed result:</p>
          <p>
            <label for="salarieids">SalarieID:</label>
            <?php
              $query = "SELECT * FROM salarie";
              $result = mysql_query($query);
              if ($result) :
            ?>
            <select id="salarieids" name="salarieid">
              <?php
                while ($row = mysql_fetch_assoc($result)) {
                  echo '<option value="', $row['salaried'], '">', $row['salaried'], '</option>'; //between <option></option> tags you can output something more human-friendly (like $row['name'], if table "salaried" have one) 
                }
              ?>
            </select>
            <?php endif ?>
          </p>
          <p>
            <input type="submit" value="Sumbit my choice"/>
          </p>
        </form>
    
        <?php if isset($_POST['salaried']) : ?>
          <?php
            $query = "SELECT * FROM salarie WHERE salarieid = " . $_POST['salarieid'];
            $result = mysql_query($query);
            if ($result) :
          ?>
            <table>
              <?php
                while ($row = mysql_fetch_assoc($result)) {
                  echo '<tr>';
                  echo '<td>', $row['salaried'], '</td><td>', $row['bla-bla-bla'], '</td>' ...; // and others
                  echo '</tr>';
                }
              ?>
            </table>
          <?php endif?>
        <?php endif ?>
      </body>
    </html>
