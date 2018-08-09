# Features:

## You can use `plantuml`:

{% plantuml %}
Rush -> Haskell : Functional Programming
{% endplantuml %}

## Or you can use Katex:

Inline math: $$\int_{-\infty}^\infty g(x) dx$$


Block math:

$$
\int_{-\infty}^\infty g(x) dx
$$

Or you can embed gist:

{% gistrun id="3da10d384b86f446973ed35629dcdd41" %}
{% endgistrun %}

## You also can create exercises (in Javascript)

```
{% exercise %}
Define a variable `x` equal to 10.
{% initial %}
var x =
{% solution %}
var x = 10;
{% validation %}
assert(x == 10);
{% context %}
// This is context code available everywhere
// The user will be able to call magicFunc in his code
function magicFunc() {
    return 3;
}
{% endexercise %}
```

{% exercise %}
Define a variable `x` equal to 10.
{% initial %}
var x =
{% solution %}
var x = 10;
{% validation %}
assert(x == 10);
{% context %}
// This is context code available everywhere
// The user will be able to call magicFunc in his code
function magicFunc() {
    return 3;
}
{% endexercise %}

## Embed Youtube:


Youtube videos can be inserted into a gitbook chapter using a tag with the video id inserted inbetween:

```
{%youtube%}dQw4w9WgXcQ{%endyoutube%}
```

You can also add a time marker in your text. Upon clicking it, the video will travel to the time specified by the marker.

{%youtube%}dQw4w9WgXcQ{%endyoutube%}

## ABC Music

{% abc %} 
X:1
T:Notes
M:C
L:1/4
K:C
C, D, E, F,|G, A, B, C|D E F G|A B c d|e f g a|b c' d' e'|f' g' a' b'|]
{% endabc %}