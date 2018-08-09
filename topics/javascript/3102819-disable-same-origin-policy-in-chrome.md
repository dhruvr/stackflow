
# Disable same origin policy in Chrome

## Question
        
Is there any way to disable the [Same-origin policy](https://en.wikipedia.org/wiki/Same_origin_policy) on Google's [Chrome](http://en.wikipedia.org/wiki/Google_Chrome) browser?

This is strictly for development, not production use.

## Answer
        
Close chrome (or chromium) and restart with the `--disable-web-security` argument. I just tested this and verified that I can access the contents of an iframe with src="http://google.com" embedded in a page served from "localhost" (tested under chromium 5 / ubuntu). For me the exact command was:

**Note : Kill all chrome instances before running command**

    chromium-browser --disable-web-security --user-data-dir="[some directory here]"
    

The browser will warn you that "you are using an unsupported command line" when it first opens, which you can ignore.

From the chromium source:

    // Don't enforce the same-origin policy. (Used by people testing their sites.)
    const wchar_t kDisableWebSecurity[] = L"disable-web-security";
    

* * *

Before Chrome 48, you could just use:

    chromium-browser --disable-web-security
