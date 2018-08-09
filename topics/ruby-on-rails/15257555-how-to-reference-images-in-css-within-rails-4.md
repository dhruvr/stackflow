
# How to reference images in CSS within Rails 4

## Question
        
There's a strange issue with Rails 4 on Heroku. When images are compiled they have hashes added to them, yet the reference to those files from within CSS don't have the proper name adjusted. Here's what I mean. I have a file called logo.png. Yet when it shows up on heroku it is viewed as:

    /assets/logo-200a00a193ed5e297bb09ddd96afb953.png
    

However the CSS still states:

    background-image:url("./logo.png");
    

The result: the image doesn't display. Anybody run into this? How can this be resolved?

## Answer
        
Sprockets together with Sass has [some nifty helpers](http://guides.rubyonrails.org/asset_pipeline.html#css-and-sass) you can use to get the job done. Sprockets will _only_ process these helpers if your stylesheet file extensions are either `.css.scss` or `.css.sass`.

* * *

**Image specific helper:**

    background-image: image-url("logo.png")
    

* * *

**Agnostic helper:**

    background-image: asset-url("logo.png", image)
    background-image: asset-url($asset, $asset-type)
    

* * *

Or if you want to embed the image data in the css file:

    background-image: asset-data-url("logo.png")
