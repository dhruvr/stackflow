
# Rails nested form with has_many :through, how to edit attributes of join model?

## Question
        
How do you edit the attributes of a join model when using accepts\_nested\_attributes_for?

I have 3 models: Topics and Articles joined by Linkers

    class Topic < ActiveRecord::Base
      has_many :linkers
      has_many :articles, :through => :linkers, :foreign_key => :article_id
      accepts_nested_attributes_for :articles
    end
    class Article < ActiveRecord::Base
      has_many :linkers
      has_many :topics, :through => :linkers, :foreign_key => :topic_id
    end
    class Linker < ActiveRecord::Base
      #this is the join model, has extra attributes like "relevance"
      belongs_to :topic
      belongs_to :article
    end
    

So when I build the article in the "new" action of the topics controller...

    @topic.articles.build
    

...and make the nested form in topics/new.html.erb...

    <% form_for(@topic) do |topic_form| %>
      ...fields...
      <% topic_form.fields_for :articles do |article_form| %>
        ...fields...
    

...Rails automatically creates the linker, which is great. **_Now for my question:_** My Linker model also has attributes that I want to be able to change via the "new topic" form. But the linker that Rails automatically creates has nil values for all its attributes except topic\_id and article\_id. How can I put fields for those other linker attributes into the "new topic" form so they don't come out nil?

## Answer
        
Figured out the answer. The trick was:

    @topic.linkers.build.build_article
    

That builds the linkers, then builds the article for each linker. So, in the models:  
topic.rb needs `accepts_nested_attributes_for :linkers`  
linker.rb needs `accepts_nested_attributes_for :article`

Then in the form:

    <%= form_for(@topic) do |topic_form| %>
      ...fields...
      <%= topic_form.fields_for :linkers do |linker_form| %>
        ...linker fields...
        <%= linker_form.fields_for :article do |article_form| %>
          ...article fields...
