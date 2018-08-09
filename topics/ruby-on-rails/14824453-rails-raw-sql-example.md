
# Rails raw SQL example

## Question
        
How can I convert this code to raw sql and use in rails? Because When I deploy this code in heroku,there is a request timeout error.I think this will be faster if I use raw sql.

    @payments = PaymentDetail.joins(:project).order('payment_details.created_at desc')
    @payment_errors = PaymentError.joins(:project).order('payment_errors.created_at desc')
    
    @all_payments = (@payments + @payment_errors)

## Answer
        
You can do this:

    sql = "Select * from ... your sql query here"
    records_array = ActiveRecord::Base.connection.execute(sql)
    

`records_array` would then be the result of your sql query in an array which you can iterate through.
