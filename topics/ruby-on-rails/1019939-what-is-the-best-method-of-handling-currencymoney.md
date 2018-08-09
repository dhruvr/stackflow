
# What is the best method of handling currency/money?

## Question
        
I'm working on a very basic shopping cart system.

I have a table `items` that has a column `price` of type `integer`.

I'm having trouble displaying the price value in my views for prices that include both Euros and cents. Am I missing something obvious as far as handling currency in the Rails framework is concerned?

## Answer
        
You'll probably want to use a `DECIMAL` type in your database. In your migration, do something like this:

    # precision is the total number of digits
    # scale is the number of digits to the right of the decimal point
    add_column :items, :price, :decimal, :precision => 8, :scale => 2
    

In Rails, the `:decimal` type is returned as `BigDecimal`, which is great for price calculation.

If you insist on using integers, you will have to manually convert to and from `BigDecimal`s everywhere, which will probably just become a pain.

As pointed out by mcl, to print the price, use:

    number_to_currency(price, :unit => "€")
    #=> €1,234.01
