
# &#x201C;What part of Hindley-Milner do you not understand?&#x201D;

## Question
        
I _swear_ there used to be a T-shirt for sale featuring the immortal words:

* * *

What part of

![Hindley-Milner](https://i.stack.imgur.com/xkKgE.png)

do you _not_ understand?

* * *

In my case, the answer would be... all of it!

In particular, I often see notation like this in Haskell papers, but I have no clue what any of it means. I have no idea what branch of mathematics it's supposed to be.

I recognise the letters of the Greek alphabet of course, and symbols such as "∉" (which usually means that something is not an element of a set).

On the other hand, I've never seen "⊢" before ([Wikipedia claims it might mean "partition"](https://en.wikipedia.org/wiki/List_of_mathematical_symbols)). I'm also unfamiliar with the use of the vinculum here. (Usually it denotes a fraction, but that does not _appear_ to be the case here.)

If somebody could at least tell me where to start looking to comprehend what this sea of symbols means, that would be helpful.

## Answer
        
*   The _horizontal bar_ means that "\[above\] **implies** \[below\]".
*   If there are _multiple expressions_ in \[above\], then consider them **anded** together; all of the \[above\] must be true in order to guarantee the \[below\].
*   `:` means **has type**
*   `∈` means **is in**. (Likewise `∉` means "is not in".)
*   `Γ` is usually used to refer to an **environment** or context; in this case it can be thought of as a set of type annotations, pairing an identifier with its type. Therefore `x : σ ∈ Γ` means that the environment `Γ` includes the fact that `x` has type `σ`.
*   `⊢` can be read as **proves** or determines. `Γ ⊢ x : σ` means that the environment `Γ` determines that `x` has type `σ`.
*   `,` is a way of **including** specific additional assumptions into an environment `Γ`.  
    Therefore, `Γ, x : τ ⊢ e : τ'` means that environment `Γ`, _with the additional, overriding assumption that `x` has type `τ`_, proves that `e` has type `τ'`.
