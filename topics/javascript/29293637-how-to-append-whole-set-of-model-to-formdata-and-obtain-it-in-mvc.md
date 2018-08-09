
# How to append whole set of model to formdata and obtain it in MVC

## Question
        
How do I pass a whole set model object through formdata and convert it to model type in the controller?

Below is what I've tried!

**JavaScript part:**

    model = {
                 EventFromDate: fromDate,
                 EventToDate: toDate,
                 ImageUrl: imgUrl,
                 HotNewsDesc: $("#txthtDescription").val().trim(),
            };
    formdata.append("model",model);
    

then pass it through AJAX, it will be a string, and if I check the value of `Request.Form["model"]` the result will be same, that is it will be received as string and value will be `"[object object]"`

**Is there any way to pass model through formdata and receive it in the controller?**

## Answer
        
If your view is based on a model and you have generated the controls inside `<form>` tags, then you can serialize the model to `FormData` using

    var formdata = new FormData($('form').get(0));
    

This will also include any files generated with `<input type="file" name="myImage" .../>`

and post it back using

    $.ajax({
      url: '@Url.Action("YourActionName", "YourControllerName")',
      type: 'POST',
      data: formdata,
      processData: false,
      contentType: false,         
    });
    

and in your controller

    [HttpPost]
    public ActionResult YourActionName(YourModelType model)
    {
    }
    

or (if your model does not include a property for `HttpPostedFileBase`)

    [HttpPost]
    public ActionResult YourActionName(YourModelType model, HttpPostedFileBase myImage)
    {
    }
    

If you want to add additional information that is not in the form, then you can append it using

    formdata.append('someProperty', 'SomeValue');
