
# How do I return the response from an Observable/http/async call in angular2?

## Question
        
I have service which returns an observable which does an http request to my server and gets the data. I want to use this data but I always end up getting `undefined`. What's the problem?

**Service**:

    @Injectable()
    export class EventService {
    
      constructor(private http: Http) { }
    
      getEventList(): Observable<any>{
        let headers = new Headers({ 'Content-Type': 'application/json' });
        let options = new RequestOptions({ headers: headers });
    
        return this.http.get("http://localhost:9999/events/get", options)
                    .map((res)=> res.json())
                    .catch((err)=> err)
      }
    }
    

**Component:**

    @Component({...})
    export class EventComponent {
    
      myEvents: any;
    
      constructor( private es: EventService ) { }
    
      ngOnInit(){
        this.es.getEventList()
            .subscribe((response)=>{
                this.myEvents = response;
            });
    
        console.log(this.myEvents); //This prints undefined!
      }
    }

## Answer
        
**Reason:**

The reason that it's `undefined` is that you are making an asynchronous operation. Meaning it'll take some time to complete the `getEventList` method (depending mostly on your network speed).

So lets look at the http call.

    this.es.getEventList()
    

After you actually make ("fire") your http request with `subscribe` you will be **waiting** for the response. While waiting, javascript will execute the lines below this code and if it encounters synchronous assignments/operations it'll execute them immediately.

So after subscribing to the `getEventList()` and waiting for the response,

`console.log(this.myEvents);`

line will be executed immediately. And the value of it is `undefined` before the response arrives from the server (or to whatever that you have initialized it in the first place).

It is similar to doing:

    ngOnInit(){
        setTimeout(()=>{
            this.myEvents = response;
        }, 5000);
    
        console.log(this.myEvents); //This prints undefined!
    }
    

* * *

**Solution:**

> So how do we overcome this problem? We will use the callback function which is the `subscribe` method. Because when the data arrives from the server it'll be inside the `subscribe` with the response.

So changing the code to:

    this.es.getEventList()
        .subscribe((response)=>{
            this.myEvents = response;
            console.log(this.myEvents); //<-- not undefined anymore
        });
    

will print the response.. after some time.

* * *

**What you should do:**

There might be lots of things to do with your response other than just logging it; you should do all these operations inside the callback (inside the `subscribe` function), when the data arrives.

Another thing to mention is that if you come from a `Promise` background, the `then` callback corresponds to `subscribe` with observables.

* * *

**What you shouldn't do:**

You shouldn't try to change an async operation to a sync operation (not that you can). One of the reasons that we have async operations is to not make the user wait for an operation to complete while they can do other things in that time period. Suppose that one of your async operations takes 3 minutes to complete, if we didn't have the async operations the interface would froze for 3 minutes.

* * *

**Suggested Reading:**

The original credit to this answer goes to: [How do I return the response from an asynchronous call?](https://stackoverflow.com/questions/14220321/how-do-i-return-the-response-from-an-asynchronous-call)

But with the angular2 release we were introduced to typescript and observables so this answer hopefully covers the basics of handling an asynchronous request with observables.
