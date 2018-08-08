
# Why do we need middleware for async flow in Redux?

## Question
      
According to the docs, ["Without middleware, Redux store only supports synchronous data flow"](http://redux.js.org/docs/advanced/AsyncFlow.html). I don't understand why this is the case. Why can't the container component call the async API, and then `dispatch` the actions?

For example, imagine a simple UI: a field and a button. When user pushes the button, the field gets populated with data from a remote server.

[![A field and a button](https://i.stack.imgur.com/GBI59.png)](https://i.stack.imgur.com/GBI59.png)

    import * as React from 'react';
    import * as Redux from 'redux';
    import { Provider, connect } from 'react-redux';
    
    const ActionTypes = {
        STARTED_UPDATING: 'STARTED_UPDATING',
        UPDATED: 'UPDATED'
    };
    
    class AsyncApi {
        static getFieldValue() {
            const promise = new Promise((resolve) => {
                setTimeout(() => {
                    resolve(Math.floor(Math.random() * 100));
                }, 1000);
            });
            return promise;
        }
    }
    
    class App extends React.Component {
        render() {
            return (
                <div>
                    <input value={this.props.field}/>
                    <button disabled={this.props.isWaiting} onClick={this.props.update}>Fetch</button>
                    {this.props.isWaiting && <div>Waiting...</div>}
                </div>
            );
        }
    }
    App.propTypes = {
        dispatch: React.PropTypes.func,
        field: React.PropTypes.any,
        isWaiting: React.PropTypes.bool
    };
    
    const reducer = (state = { field: 'No data', isWaiting: false }, action) => {
        switch (action.type) {
            case ActionTypes.STARTED_UPDATING:
                return { ...state, isWaiting: true };
            case ActionTypes.UPDATED:
                return { ...state, isWaiting: false, field: action.payload };
            default:
                return state;
        }
    };
    const store = Redux.createStore(reducer);
    const ConnectedApp = connect(
        (state) => {
            return { ...state };
        },
        (dispatch) => {
            return {
                update: () => {
                    dispatch({
                        type: ActionTypes.STARTED_UPDATING
                    });
                    AsyncApi.getFieldValue()
                        .then(result => dispatch({
                            type: ActionTypes.UPDATED,
                            payload: result
                        }));
                }
            };
        })(App);
    export default class extends React.Component {
        render() {
            return <Provider store={store}><ConnectedApp/></Provider>;
        }
    }
    

When the exported component is rendered, I can click the button and the input is updated correctly.

Note the `update` function in the `connect` call. It dispatches an action that tells the App that it is updating, and then performs an async call. After the call finishes, the provided value is dispatched as a payload of another action.

What is wrong with this approach? Why would I want to use Redux Thunk or Redux Promise, as the documentation suggests?

**EDIT:** I searched the Redux repo for clues, and found that Action Creators were required to be pure functions in the past. For example, [here's a user trying to provide a better explanation for async data flow:](https://github.com/reactjs/redux/issues/533)

> The action creator itself is still a pure function, but the thunk function it returns doesn't need to be, and it can do our async calls

[Action creators are no longer required to be pure.](https://github.com/reactjs/redux/issues/1088) So, thunk/promise middleware was definitely required in the past, but it seems that this is no longer the case?
## Answer
      
> What is wrong with this approach? Why would I want to use Redux Thunk or Redux Promise, as the documentation suggests?

There is nothing wrong with this approach. It’s just inconvenient in a large application because you’ll have different components performing the same actions, you might want to debounce some actions, or keep some local state like auto-incrementing IDs close to action creators, etc. So it is just easier from the maintenance point of view to extract action creators into separate functions.

**You can read [my answer to “How to dispatch a Redux action with a timeout”](https://stackoverflow.com/questions/35411423/how-to-dispatch-a-redux-action-with-a-timeout/35415559#35415559) for a more detailed walkthrough.**

Middleware like Redux Thunk or Redux Promise just gives you “syntax sugar” for dispatching thunks or promises, but you don’t _have to_ use it.

So, without any middleware, your action creator might look like

    // action creator
    function loadData(dispatch, userId) { // needs to dispatch, so it is first argument
      return fetch(`http://data.com/${userId}`)
        .then(res => res.json())
        .then(
          data => dispatch({ type: 'LOAD_DATA_SUCCESS', data }),
          err => dispatch({ type: 'LOAD_DATA_FAILURE', err })
        );
    }
    
    // component
    componentWillMount() {
      loadData(this.props.dispatch, this.props.userId); // don't forget to pass dispatch
    }
    

But with Thunk Middleware you can write it like this:

    // action creator
    function loadData(userId) {
      return dispatch => fetch(`http://data.com/${userId}`) // Redux Thunk handles these
        .then(res => res.json())
        .then(
          data => dispatch({ type: 'LOAD_DATA_SUCCESS', data }),
          err => dispatch({ type: 'LOAD_DATA_FAILURE', err })
        );
    }
    
    // component
    componentWillMount() {
      this.props.dispatch(loadData(this.props.userId)); // dispatch like you usually do
    }
    

So there is no huge difference. One thing I like about the latter approach is that the component doesn’t care that the action creator is async. It just calls `dispatch` normally, it can also use `mapDispatchToProps` to bind such action creator with a short syntax, etc. The components don’t know how action creators are implemented, and you can switch between different async approaches (Redux Thunk, Redux Promise, Redux Saga) without changing the components. On the other hand, with the former, explicit approach, your components know _exactly_ that a specific call is async, and needs `dispatch` to be passed by some convention (for example, as a sync parameter).

Also think about how this code will change. Say we want to have a second data loading function, and to combine them in a single action creator.

With the first approach we need to be mindful of what kind of action creator we are calling:

    // action creators
    function loadSomeData(dispatch, userId) {
      return fetch(`http://data.com/${userId}`)
        .then(res => res.json())
        .then(
          data => dispatch({ type: 'LOAD_SOME_DATA_SUCCESS', data }),
          err => dispatch({ type: 'LOAD_SOME_DATA_FAILURE', err })
        );
    }
    function loadOtherData(dispatch, userId) {
      return fetch(`http://data.com/${userId}`)
        .then(res => res.json())
        .then(
          data => dispatch({ type: 'LOAD_OTHER_DATA_SUCCESS', data }),
          err => dispatch({ type: 'LOAD_OTHER_DATA_FAILURE', err })
        );
    }
    function loadAllData(dispatch, userId) {
      return Promise.all(
        loadSomeData(dispatch, userId), // pass dispatch first: it's async
        loadOtherData(dispatch, userId) // pass dispatch first: it's async
      );
    }
    
    
    // component
    componentWillMount() {
      loadAllData(this.props.dispatch, this.props.userId); // pass dispatch first
    }
    

With Redux Thunk action creators can `dispatch` the result of other action creators and not even think whether those are synchronous or asynchronous:

    // action creators
    function loadSomeData(userId) {
      return dispatch => fetch(`http://data.com/${userId}`)
        .then(res => res.json())
        .then(
          data => dispatch({ type: 'LOAD_SOME_DATA_SUCCESS', data }),
          err => dispatch({ type: 'LOAD_SOME_DATA_FAILURE', err })
        );
    }
    function loadOtherData(userId) {
      return dispatch => fetch(`http://data.com/${userId}`)
        .then(res => res.json())
        .then(
          data => dispatch({ type: 'LOAD_OTHER_DATA_SUCCESS', data }),
          err => dispatch({ type: 'LOAD_OTHER_DATA_FAILURE', err })
        );
    }
    function loadAllData(userId) {
      return dispatch => Promise.all(
        dispatch(loadSomeData(userId)), // just dispatch normally!
        dispatch(loadOtherData(userId)) // just dispatch normally!
      );
    }
    
    
    // component
    componentWillMount() {
      this.props.dispatch(loadAllData(this.props.userId)); // just dispatch normally!
    }
    

With this approach, if you later want your action creators to look into current Redux state, you can just use the second `getState` argument passed to the thunks without modifying the calling code at all:

    function loadSomeData(userId) {
      // Thanks to Redux Thunk I can use getState() here without changing callers
      return (dispatch, getState) => {
        if (getState().data[userId].isLoaded) {
          return Promise.resolve();
        }
    
        fetch(`http://data.com/${userId}`)
          .then(res => res.json())
          .then(
            data => dispatch({ type: 'LOAD_SOME_DATA_SUCCESS', data }),
            err => dispatch({ type: 'LOAD_SOME_DATA_FAILURE', err })
          );
      }
    }
    

If you need to change it to be synchronous, you can also do this without changing any calling code:

    // I can change it to be a regular action creator without touching callers
    function loadSomeData(userId) {
      return {
        type: 'LOAD_SOME_DATA_SUCCESS',
        data: localStorage.getItem('my-data')
      }
    }
    

So the benefit of using middleware like Redux Thunk or Redux Promise is that components aren’t aware of how action creators are implemented, and whether they care about Redux state, whether they are synchronous or asynchronous, and whether or not they call other action creators. The downside is a little bit of indirection, but we believe it’s worth it in real applications.

Finally, Redux Thunk and friends is just one possible approach to asynchronous requests in Redux apps. Another interesting approach is [Redux Saga](https://github.com/yelouafi/redux-saga) which lets you define long-running daemons (“sagas”) that take actions as they come, and transform or perform requests before outputting actions. This moves the logic from action creators into sagas. You might want to check it out, and later pick what suits you the most.

> I searched the Redux repo for clues, and found that Action Creators were required to be pure functions in the past.

This is incorrect. The docs said this, but the docs were wrong.  
Action creators were never required to be pure functions.  
We fixed the docs to reflect that.
    