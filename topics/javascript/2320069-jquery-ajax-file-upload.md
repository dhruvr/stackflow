
# jQuery Ajax File Upload

## Question
        
Can I use the following jQuery code to perform file upload using post method of an Ajax request ?

    $.ajax({
        type: "POST",
        timeout: 50000,
        url: url,
        data: dataString,
        success: function (data) {
            alert('success');
            return false;
        }
    });
    

If it is possible, do I need to fill "data" part? Is it the correct way? I only post the file to the server side.

I have been Googling around, but what I found was a plugin while in my plan I do not want to use it. At least for the moment.

## Answer
        
file upload is not possible through ajax. You can upload file, without refreshing page by using IFrame. you can check further detail [here](http://www.ajaxf1.com/tutorial/ajax-file-upload-tutorial.html)

**UPDATE:**

With XHR2, File upload through AJAX is supported. E.g. through [FormData](https://developer.mozilla.org/en-US/docs/Web/Guide/Using_FormData_Objects) object, but unfortunately it is not supported by all/old browsers.

FormData support starts from following desktop browsers versions. IE 10+, Firefox 4.0+, Chrome 7+, Safari 5+, Opera 12+

For more detail, see [MDN link](https://developer.mozilla.org/en-US/docs/XMLHttpRequest/FormData)
