
# Nested attributes unpermitted parameters

## Question
        
I have a `Bill` object, which has many `Due` objects. The `Due` object also belongs to a `Person`. I want a form that can create the `Bill` and its children `Dues` all in one page. I am trying to create a form using nested attributes, similar to ones in [this Railscast](http://railscasts.com/episodes/196-nested-model-form-part-1).

Relevant code is listed below:

**due.rb**

    class Due < ActiveRecord::Base
        belongs_to :person
        belongs_to :bill
    end
    

**bill.rb**

    class Bill < ActiveRecord::Base
        has_many :dues, :dependent => :destroy 
        accepts_nested_attributes_for :dues, :allow_destroy => true
    end
    

**bills_controller.rb**

      # GET /bills/new
      def new
          @bill = Bill.new
          3.times { @bill.dues.build }
      end
    

**bills/_form.html.erb**

      <%= form_for(@bill) do |f| %>
        <div class="field">
            <%= f.label :company %><br />
            <%= f.text_field :company %>
        </div>
        <div class="field">
            <%= f.label :month %><br />
            <%= f.text_field :month %>
        </div>
        <div class="field">
            <%= f.label :year %><br />
            <%= f.number_field :year %>
        </div>
        <div class="actions">
            <%= f.submit %>
        </div>
        <%= f.fields_for :dues do |builder| %>
            <%= render 'due_fields', :f => builder %>
        <% end %>
      <% end %>
    

**bills/\_due\_fields.html.erb**

    <div>
        <%= f.label :amount, "Amount" %>        
        <%= f.text_field :amount %>
        <br>
        <%= f.label :person_id, "Renter" %>
        <%= f.text_field :person_id %>
    </div>
    

**UPDATE to bills_controller.rb** This works!

    def bill_params 
      params
      .require(:bill)
      .permit(:company, :month, :year, dues_attributes: [:amount, :person_id]) 
    end
    

The proper fields are rendered on the page (albeit without a dropdown for `Person` yet) and submit is successful. However, none of the children dues are saved to the database, and an error is thrown in the server log:

    Unpermitted parameters: dues_attributes
    

Just before the error, the log displays this:

    Started POST "/bills" for 127.0.0.1 at 2013-04-10 00:16:37 -0700
    Processing by BillsController#create as HTML<br>
    Parameters: {"utf8"=>"✓", 
    "authenticity_token"=>"ipxBOLOjx68fwvfmsMG3FecV/q/hPqUHsluBCPN2BeU=",
     "bill"=>{"company"=>"Comcast", "month"=>"April ", 
    "year"=>"2013", "dues_attributes"=>{
    "0"=>{"amount"=>"30", "person_id"=>"1"}, 
    "1"=>{"amount"=>"30", "person_id"=>"2"},
     "2"=>{"amount"=>"30", "person_id"=>"3"}}}, "commit"=>"Create Bill"}
    

Has there been some change in Rails 4?

## Answer
        
Seems there is a change in handling of attribute protection and now you must whitelist params in the controller (instead of attr\_accessible in the model) because the former optional gem strong\_parameters became part of the Rails Core.

This should look something like this:

    class PeopleController < ActionController::Base
      def create
        Person.create(person_params)
      end
    
    private
      def person_params
        params.require(:person).permit(:name, :age)
      end
    end
    

So `params.require(:model).permit(:fields)` would be used

and for nested attributes something like

    params.require(:person).permit(:name, :age, pets_attributes: [:id, :name, :category])
    

Some more details can be found in the [Ruby edge API docs](http://edgeapi.rubyonrails.org/classes/ActionController/StrongParameters.html) and [strong_parameters on github](https://github.com/rails/strong_parameters) or [here](http://rubysource.com/rails-4-quick-look-strong-parameters/)
