
# &#x201C;Thinking in AngularJS&#x201D; if I have a jQuery background? [closed]

## Question
        
Suppose I'm familiar with developing client-side applications in [jQuery](http://jquery.com/), but now I'd like to start using [AngularJS](http://angularjs.org/). Can you describe the paradigm shift that is necessary? Here are a few questions that might help you frame an answer:

*   How do I architect and design client-side web applications differently? What is the biggest difference?
*   What should I stop doing/using; What should I start doing/using instead?
*   Are there any server-side considerations/restrictions?

I'm not looking for a detailed comparison between `jQuery` and `AngularJS`.

## Answer
        
1\. Don't design your page, and then change it with [DOM](http://en.wikipedia.org/wiki/Document_Object_Model) manipulations
===========================================================================================================================

In jQuery, you design a page, and then you make it dynamic. This is because jQuery was designed for augmentation and has grown incredibly from that simple premise.

But in AngularJS, you must start from the ground up with your architecture in mind. Instead of starting by thinking "I have this piece of the DOM and I want to make it do X", you have to start with what you want to accomplish, then go about designing your application, and then finally go about designing your view.

2\. Don't augment jQuery with AngularJS
=======================================

Similarly, don't start with the idea that jQuery does X, Y, and Z, so I'll just add AngularJS on top of that for models and controllers. This is _really_ tempting when you're just starting out, which is why I always recommend that new AngularJS developers don't use jQuery at all, at least until they get used to doing things the "Angular Way".

I've seen many developers here and on the mailing list create these elaborate solutions with jQuery plugins of 150 or 200 lines of code that they then glue into AngularJS with a collection of callbacks and `$apply`s that are confusing and convoluted; but they eventually get it working! The problem is that in **most** cases that jQuery plugin could be rewritten in AngularJS in a fraction of the code, where suddenly everything becomes comprehensible and straightforward.

The bottom line is this: when solutioning, first "think in AngularJS"; if you can't think of a solution, ask the community; if after all of that there is no easy solution, _then_ feel free to reach for the jQuery. But don't let jQuery become a crutch or you'll never master AngularJS.

3\. Always think in terms of architecture
=========================================

First know that [single-page applications](http://en.wikipedia.org/wiki/Single-page_application) are _applications_. They're _not_ webpages. So we need to think like a server-side developer _in addition_ to thinking like a client-side developer. We have to think about how to divide our application into individual, extensible, testable components.

So then _how_ do you do that? How do you "think in AngularJS"? Here are some general principles, contrasted with jQuery.

The view is the "official record"
---------------------------------

In jQuery, we programmatically change the view. We could have a dropdown menu defined as a `ul` like so:

    <ul class="main-menu">
        <li class="active">
            <a href="#/home">Home</a>
        </li>
        <li>
            <a href="#/menu1">Menu 1</a>
            <ul>
                <li><a href="#/sm1">Submenu 1</a></li>
                <li><a href="#/sm2">Submenu 2</a></li>
                <li><a href="#/sm3">Submenu 3</a></li>
            </ul>
        </li>
        <li>
            <a href="#/home">Menu 2</a>
        </li>
    </ul>
    

In jQuery, in our application logic, we would activate it with something like:

    $('.main-menu').dropdownMenu();
    

When we just look at the view, it's not immediately obvious that there is any functionality here. For small applications, that's fine. But for non-trivial applications, things quickly get confusing and hard to maintain.

In AngularJS, though, the view is the official record of view-based functionality. Our `ul` declaration would look like this instead:

    <ul class="main-menu" dropdown-menu>
        ...
    </ul>
    

These two do the same thing, but in the AngularJS version anyone looking at the template knows what's supposed to happen. Whenever a new member of the development team comes on board, she can look at this and then _know_ that there is a directive called `dropdownMenu` operating on it; she doesn't need to intuit the right answer or sift through any code. The view told us what was supposed to happen. Much cleaner.

Developers new to AngularJS often ask a question like: how do I find all links of a specific kind and add a directive onto them. The developer is always flabbergasted when we reply: you don't. But the reason you don't do that is that this is like half-jQuery, half-AngularJS, and no good. The problem here is that the developer is trying to "do jQuery" in the context of AngularJS. That's never going to work well. The view _is_ the official record. Outside of a directive (more on this below), you never, ever, _never_ change the DOM. And directives are applied _in the view_, so intent is clear.

Remember: don't design, and then mark up. You must architect, and then design.

Data binding
------------

This is by far one of the most awesome features of AngularJS and cuts out a lot of the need to do the kinds of DOM manipulations I mentioned in the previous section. AngularJS will automatically update your view so you don't have to! In jQuery, we respond to events and then update content. Something like:

    $.ajax({
      url: '/myEndpoint.json',
      success: function ( data, status ) {
        $('ul#log').append('<li>Data Received!</li>');
      }
    });
    

For a view that looks like this:

    <ul class="messages" id="log">
    </ul>
    

Apart from mixing concerns, we also have the same problems of signifying intent that I mentioned before. But more importantly, we had to manually reference and update a DOM node. And if we want to delete a log entry, we have to code against the DOM for that too. How do we test the logic apart from the DOM? And what if we want to change the presentation?

This a little messy and a trifle frail. But in AngularJS, we can do this:

    $http( '/myEndpoint.json' ).then( function ( response ) {
        $scope.log.push( { msg: 'Data Received!' } );
    });
    

And our view can look like this:

    <ul class="messages">
        <li ng-repeat="entry in log">{{ entry.msg }}</li>
    </ul>
    

But for that matter, our view could look like this:

    <div class="messages">
        <div class="alert" ng-repeat="entry in log">
            {{ entry.msg }}
        </div>
    </div>
    

And now instead of using an unordered list, we're using Bootstrap alert boxes. And we never had to change the controller code! But more importantly, no matter _where_ or _how_ the log gets updated, the view will change too. Automatically. Neat!

Though I didn't show it here, the data binding is two-way. So those log messages could also be editable in the view just by doing this: `<input ng-model="entry.msg" />`. And there was much rejoicing.

Distinct model layer
--------------------

In jQuery, the DOM is kind of like the model. But in AngularJS, we have a separate model layer that we can manage in any way we want, completely independently from the view. This helps for the above data binding, maintains [separation of concerns](http://en.wikipedia.org/wiki/Separation_of_concerns), and introduces far greater testability. Other answers mentioned this point, so I'll just leave it at that.

Separation of concerns
----------------------

And all of the above tie into this over-arching theme: keep your concerns separate. Your view acts as the official record of what is supposed to happen (for the most part); your model represents your data; you have a service layer to perform reusable tasks; you do DOM manipulation and augment your view with directives; and you glue it all together with controllers. This was also mentioned in other answers, and the only thing I would add pertains to testability, which I discuss in another section below.

Dependency injection
--------------------

To help us out with separation of concerns is [dependency injection](http://en.wikipedia.org/wiki/Dependency_injection) (DI). If you come from a server-side language (from [Java](http://en.wikipedia.org/wiki/Java_%28programming_language%29) to [PHP](http://en.wikipedia.org/wiki/PHP)) you're probably familiar with this concept already, but if you're a client-side guy coming from jQuery, this concept can seem anything from silly to superfluous to hipster. But it's not. :-)

From a broad perspective, DI means that you can declare components very freely and then from any other component, just ask for an instance of it and it will be granted. You don't have to know about loading order, or file locations, or anything like that. The power may not immediately be visible, but I'll provide just one (common) example: testing.

Let's say in our application, we require a service that implements server-side storage through a [REST](http://en.wikipedia.org/wiki/Representational_State_Transfer) API and, depending on application state, local storage as well. When running tests on our controllers, we don't want to have to communicate with the server - we're testing the _controller_, after all. We can just add a mock service of the same name as our original component, and the injector will ensure that our controller gets the fake one automatically - our controller doesn't and needn't know the difference.

Speaking of testing...

4\. Test-driven development - _always_
======================================

This is really part of section 3 on architecture, but it's so important that I'm putting it as its own top-level section.

Out of all of the many jQuery plugins you've seen, used, or written, how many of them had an accompanying test suite? Not very many because jQuery isn't very amenable to that. But AngularJS is.

In jQuery, the only way to test is often to create the component independently with a sample/demo page against which our tests can perform DOM manipulation. So then we have to develop a component separately and _then_ integrate it into our application. How inconvenient! So much of the time, when developing with jQuery, we opt for iterative instead of test-driven development. And who could blame us?

But because we have separation of concerns, we can do test-driven development iteratively in AngularJS! For example, let's say we want a super-simple directive to indicate in our menu what our current route is. We can declare what we want in the view of our application:

    <a href="/hello" when-active>Hello</a>
    

Okay, now we can write a test for the non-existent `when-active` directive:

    it( 'should add "active" when the route changes', inject(function() {
        var elm = $compile( '<a href="/hello" when-active>Hello</a>' )( $scope );
    
        $location.path('/not-matching');
        expect( elm.hasClass('active') ).toBeFalsey();
    
        $location.path( '/hello' );
        expect( elm.hasClass('active') ).toBeTruthy();
    }));
    

And when we run our test, we can confirm that it fails. Only now should we create our directive:

    .directive( 'whenActive', function ( $location ) {
        return {
            scope: true,
            link: function ( scope, element, attrs ) {
                scope.$on( '$routeChangeSuccess', function () {
                    if ( $location.path() == element.attr( 'href' ) ) {
                        element.addClass( 'active' );
                    }
                    else {
                        element.removeClass( 'active' );
                    }
                });
            }
        };
    });
    

Our test now passes _and_ our menu performs as requested. Our development is _both_ iterative _and_ test-driven. Wicked-cool.

5\. Conceptually, directives are _not_ packaged jQuery
======================================================

You'll often hear "only do DOM manipulation in a directive". **This is a necessity.** Treat it with due deference!

But let's dive a little deeper...

Some directives just decorate what's already in the view (think `ngClass`) and therefore sometimes do DOM manipulation straight away and then are basically done. But if a directive is like a "widget" and has a template, it should _also_ respect separation of concerns. That is, the template _too_ should remain largely independent from its implementation in the link and controller functions.

AngularJS comes with an entire set of tools to make this very easy; with `ngClass` we can dynamically update the class; `ngModel` allows two-way data binding; `ngShow` and `ngHide` programmatically show or hide an element; and many more - including the ones we write ourselves. In other words, we can do all kinds of awesomeness _without_ DOM manipulation. The less DOM manipulation, the easier directives are to test, the easier they are to style, the easier they are to change in the future, and the more re-usable and distributable they are.

I see lots of developers new to AngularJS using directives as the place to throw a bunch of jQuery. In other words, they think "since I can't do DOM manipulation in the controller, I'll take that code put it in a directive". While that certainly is much better, it's often _still wrong_.

Think of the logger we programmed in section 3. Even if we put that in a directive, we _still_ want to do it the "Angular Way". It _still_ doesn't take any DOM manipulation! There are lots of times when DOM manipulation is necessary, but it's a _lot_ rarer than you think! Before doing DOM manipulation _anywhere_ in your application, ask yourself if you really need to. There might be a better way.

Here's a quick example that shows the pattern I see most frequently. We want a toggleable button. (Note: this example is a little contrived and a skosh verbose to represent more complicated cases that are solved in exactly the same way.)

    .directive( 'myDirective', function () {
        return {
            template: '<a class="btn">Toggle me!</a>',
            link: function ( scope, element, attrs ) {
                var on = false;
    
                $(element).click( function () {
                    on = !on;
                    $(element).toggleClass('active', on);
                });
            }
        };
    });
    

There are a few things wrong with this:

1.  First, jQuery was never necessary. There's nothing we did here that needed jQuery at all!
2.  Second, even if we already have jQuery on our page, there's no reason to use it here; we can simply use `angular.element` and our component will still work when dropped into a project that doesn't have jQuery.
3.  Third, even assuming jQuery _was_ required for this directive to work, jqLite (`angular.element`) will _always_ use jQuery if it was loaded! So we needn't use the `$` \- we can just use `angular.element`.
4.  Fourth, closely related to the third, is that jqLite elements needn't be wrapped in `$` \- the `element` that is passed to the `link` function would _already be_ a jQuery element!
5.  And fifth, which we've mentioned in previous sections, why are we mixing template stuff into our logic?

This directive can be rewritten (even for very complicated cases!) much more simply like so:

    .directive( 'myDirective', function () {
        return {
            scope: true,
            template: '<a class="btn" ng-class="{active: on}" ng-click="toggle()">Toggle me!</a>',
            link: function ( scope, element, attrs ) {
                scope.on = false;
    
                scope.toggle = function () {
                    scope.on = !scope.on;
                };
            }
        };
    });
    

Again, the template stuff is in the template, so you (or your users) can easily swap it out for one that meets any style necessary, and the **logic** never had to be touched. Reusability - boom!

And there are still all those other benefits, like testing - it's easy! No matter what's in the template, the directive's internal API is never touched, so refactoring is easy. You can change the template as much as you want without touching the directive. And no matter what you change, your tests still pass.

w00t!

So if directives aren't just collections of jQuery-like functions, what are they? Directives are actually **extensions of HTML**. If HTML doesn't do something you need it to do, you write a directive to do it for you, and then use it just as if it was part of HTML.

Put another way, if AngularJS doesn't do something out of the box, think how the team would accomplish it to fit right in with `ngClick`, `ngClass`, et al.

Summary
=======

Don't even use jQuery. Don't even include it. It will hold you back. And when you come to a problem that you think you know how to solve in jQuery already, before you reach for the `$`, try to think about how to do it within the confines the AngularJS. If you don't know, ask! 19 times out of 20, the best way to do it doesn't need jQuery and to try to solve it with jQuery results in more work for you.
