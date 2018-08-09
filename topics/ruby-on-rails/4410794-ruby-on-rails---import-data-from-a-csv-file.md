
# Ruby on Rails - Import Data from a CSV file

## Question
        
I would like to import data from a CSV file into an existing database table. I do not want to save the CSV file, just take the data from it and put it into the existing table. I am using Ruby 1.9.2 and Rails 3.

This is my table:

    create_table "mouldings", :force => true do |t|
      t.string   "suppliers_code"
      t.datetime "created_at"
      t.datetime "updated_at"
      t.string   "name"
      t.integer  "supplier_id"
      t.decimal  "length",         :precision => 3, :scale => 2
      t.decimal  "cost",           :precision => 4, :scale => 2
      t.integer  "width"
      t.integer  "depth"
    end
    

Can you give me some code to show me the best way to do this, thanks.

## Answer
        
    require 'csv'    
    
    csv_text = File.read('...')
    csv = CSV.parse(csv_text, :headers => true)
    csv.each do |row|
      Moulding.create!(row.to_hash)
    end
