# Fωμ type checker &mdash; [![Build Status](https://travis-ci.org/polytypic/f-omega-mu.svg?branch=main)](https://travis-ci.org/polytypic/f-omega-mu)

A type checker for Fωμ _restricted to non-nested types_.

This is basically a generalization of the type system for Fωμ\*, that is Fωμ
_restricted to first-order recursive types_ of kind `*`, described in the
article

<blockquote>
  <dl>
    <dt>
      <a href="http://ps.informatik.uni-tuebingen.de/research/functors/equirecursion-fomega-popl16.pdf">
        System F-omega with Equirecursive Types for Datatype-generic Programming
      </a>
    </dt>
    <dd>by Yufei Cai, Paolo G. Giarrusso, and Klaus Ostermann.</dd>
  </dl>
</blockquote>

While Fωμ\* is powerful enough to express regular datatypes, it requires type
parameters to be hoisted outside of the `μ`. For example, the list type

```
μlist:* → *.λα:*.opt (α, list α)
```

needs to be rewritten as

```
λα:*.μlist_1:*.opt (α, list_1)
```

Both of the above types are allowed by this generalized system and are also
considered equivalent as show in this
[example](https://polytypic.github.io/f-omega-mu/#MQAgMglgzgLiCmBHArhAbgQwDbwHYGN4AoInOGATwAd4QB7KuAXhEG7gQRuAAuAKgDoBtXHVzxOIABQBKADQgodALaiQ7ALogIuEmRCUaILNDggQLQD3Ah2DxCAkwhB8OPXgzjj2sy3HaSNWnXtpPAH0ARlM2Lj4LI1CnFwl3AxiQn01teDg6ADMgzxNwwGzgSN5WAA9OPJN2XlLfUgz6HOCwliKnMorklRq6+rh8AAt4fABrQuLWTzE86snk6a7qohNxbNyjfjUk2FDZNebN9U9UrSJB4ZGgA).

In this generalized system, nested types are not allowed. For example,

```
μnested:* → *.λα:*.(α, nested (α, α))
```

is disallowed due to the argument `(α, α)` as demonstrated in this
[example](https://polytypic.github.io/f-omega-mu/#MQAgcgpgzgLhAmJ4EMaoJ4AdomQJwiQEspkAbMgewHcEAoOsiGEAO0pgH1yrbEBeEIG7gAB4AuQD3AraHHhiAVCEBJhCHkA6IYEbgBWoAUmgDRsZCEPqOaAlJbUiQRVg12WgA).

Disallowing nested types is sufficient to keep the number of distinct subtrees
finite in the infinite expansions of recursive types and to keep type
equivalence decidable.

In addition, this variation also has basic structural subtyping with joins (and
meets) and without bounded quantification. The motivation for providing
subtyping is to allow one to better exploit untyped compilation targets such as
JavaScript without having to e.g. perform unnecessary coercions.

This is still very much Work-in-Progress with tons of missing features and
probably more bugs than one could imagine.

## Next steps

- Try the online playground:
  - [The traditional `fact`orial function](https://polytypic.github.io/f-omega-mu/#MQAgKgFgpiAuBOBDAJgS1qg9gO0QGxAAMAzRAY1kM3lXxGIFdsKtsAoNvKWEQHuBSKALlTYegJMIQIngF42IEIG7gbMNEA6OfMnEQ2EIBMiEAEYN82NF3HNIKHgDOMXQCp65HgApdAWiMBKSezYBHgBWIA)
  - [Iterative `fib`onacci function](https://polytypic.github.io/f-omega-mu/#MQAgkgLgpgTghhAlgNyiABgM0QI3QewDs4BjExETAV0JKSICgGAbKCS3EAXgZBFfaAe4Gb58ABwBciQu0BJhCGlyFMkPMXdefEIG7gRFJkA6bQCt9EI4TMHNWhZhCEQgEyIQABhtaIACyiPEHvihmAGc0EXEQYxAACgoAakiAShjHAFoQAEZk6U1wsTdM5SZsHEzXIA)
  - [Recursive `list` type encoding and `fold` function](https://polytypic.github.io/f-omega-mu/#MQAgSgpgxgrgTgZwJYDcIgAYBskIC4Yh4CeADuhAHZQD2AJkpQOYgCGldmAZjVnYVxjU8SGpQBQ4rBDxEy6QD3AOfAC4AVCEBJhCA0BeEIG7gPADpAAERxjAb0pIsKkHAA0IWpQT3Z25bM8OAvloOIIyS0rI2WCD6gNnAJtEWBlAq1rb2Ti5i7kSB3tnacH7GUMYRwRJhGW5RILHGBgAWdComRna5cQlJKXYOzq5Zvu2BBUVFmSCNRJEhUjIgCjx8KqYm5sYAFL5wwwCUwzm4PvsxHXVclM37+XUAXioJAB7u7cbiICBPANpwALog3dUbn1xvoDA9mnUnioXos6J88D9vn9ziB1iiHiAbnsngEZhVSEgAPoMJhIPAIar9T6MBEgADMqKpNL+AEZGZlqZRaQAWdluTm0tnrCICn47cV7Gaw0Wi1FglQ0uruRUYgDUIAQewADCACcSkKTyUA)
  - [Generic `fold`s](https://polytypic.github.io/f-omega-mu/#MQAg4gpgdhBOCWBjEADAZgewDYBMUGcAoQrCAFxDIE8AHCEAMQFcpEyNYQBeEQbuBAx4ABcAKhCAkwhDCAdAG8AtgEMagkIAAiQI3AU1YCbgKQAp14kNoCURvfxCGJl0wF8Q8KMVIVAPcCZcg1UNETpGlLMrOyclhIWVkbqZhJuFrFRXIQgIIDZwL5G0mmavAyCwWwcIPxSKal8ClgA5hAARrAKgpbWVmUVFR7YOILx-Im5AG69CVJVtQ0KIHoMUoo0ANp9JgC6C+orIJ44IINmTsSgAPI0ZPAYUJS0ECTkV3QgJxQ8vJoLAHIXECp6JgA0IAAyhg5N8rJsDoRQAAZeD4CjUOi3BHXECw+EMbh8XKAEeApE9puoATj9s5XPd6OjntipPEqZiYo4yXd6Vi0niZOV5mzNGldLw0IJWrpEAp8PROR1Up8YFjeL8pB8vljfis-uUOsDQXK9AAPQQ4gG6-BCkyKrX0Hh6gFoEDG1YakB2QgOSHkqnccrk5U8HJSXj6t4WlW6gFK2VW1arJleu5uAAWYu8mgMRjqGGwiQ9rXT2B5-uUOYzWCkovFIElUpllr4CrQVXF6qlQJBNflgaNJuzZpodpAgAoiECJ-DrTa942O13OVLk7ZswCDwFI-bxYLrYIJ59EjIuVxv2hVtgtWRsFvPNnp6aOzIBB0hAZYljo61Z1ZtgTebLe1Lz1QoB6-nr5riAnC6o+ToxjOdywBAgwAPrsPmh7ZmsGzTAGXZwhQuSBuarYhp2UYQSA5LQYM+akfBGCjiA4Y1qqRGzvAWBkHA+a8IWabFv6xqCNm+6pBOHSAO2kWzdEemHgtRejobxEnYUKUjwLa46UPG0A0cGVqhna+DRhAWDlvaNHPpGKyOiJpHUQckEUIgFyitSy48XxvBUBh8JtJZqHGiAFkwZR1FuURMhfACw42t0f4wTaTEsW+d72QoZBTocQJ3AAggAIgAKsiFJpdSvD6RAcj+uKZCyOUJWnFQKjle++BONUrgXCoxVyEY9XlNBcgYIMYLtZ1dwSF1qQsOcUB1cNIDlUNZDvuwsFYJhU0UBIHrtS6MZQiA7wKAA9-1BWOHINCkKCUBkElE15YCdw8JWigANY1mk7X+hAACOgiDRIv0gLmJaOuSiL0LNPAbedTKAFAM5UADqVh01XUFitEqqs74dI1UDNeQFxYlSUhfAs7VqmBPV9W2+rvYTaCxXAJPneerk-edUgADVfX2VD-GB434y8WCCOV-rriLYGpITdmsEljMlZsWDTLT9OwHLcjM-ArMlRzhPDmr55c-AZhYGYsC882i3LR5LwmiLRAdHYgDQDCogDARHod0UO1JhbZCoDZXAHWg3loMgP7sByAAQmKbY7oAw8D+niCzlLCZAqDIgxVCoTjJe+ABqCjrhW8A9CA852O+6V1CaFYZ4X8IIDjAIQLqyggLH5flOlNCtzIaCTSAhogAX1QqO3hAQsyKIPGHciYi8eIz1H5b101A9rwcIOojPWLxDPDCksHqIAKIZ1gADCZC6lii-R0CZAN9UocBzGm8PPnsDb-PUg3+WOJP+H-8OpujuHvLEj0lD5j5P6BQdQhRGBFLfRGaJ4CFXTpnbOdhFQpzAafOwZMOjvzlDIYuwt75NUwQsQhD1i54MxiASu+AiG11IQ-JuLchQUIYTggubDew8BgSAZuNBaGOi7nwvgvdJpEiHrAEe6hOHdzAX3LEAi+4AmHiouoMjqh4PKM6FKO0PYgAwLaFeOMiCuGhsXfAcMACSOB8COx4B7OYCgXoLDMdUc8XAPFkJxisA+O0z4ghoEwFiWxoL0FrvAGBpBGFMGxo-dA3QUB5TQGRZIqRDx7zWNYzYoDbz3grI6bBLw0FYC4LBQQGCpD2PwFIZGVA6FUIkSQzxmC6lSESS1S4xc6FcLKcwzxvDqkOI6Q4qQFMjq10ES3fpiiyl91GfgdRsjlnjPqfzS4yjh56O2qAAAqok1A6TUlJ1SGIsBjplE8HOVKAZxSPy7ALliAARLqV5dCkYt1Rpch6NyaItOITsHgryGCvNodo1GQLi5vI+Xg3RzZnQdHwRUDRtyYUgpAK8qgEKzKpGdGZES6TCAiU6ZbTCQA)
  - [List using a recursive sum type](https://polytypic.github.io/f-omega-mu/#MQAgMglgzgLiCuUIDsDmICGIBOBTAxvNkgG64hTwC2IMAngA64BQzANrnPUyAPYNwAvCEDdwIEbgAHQBtZL2S4AXCAAUASgA0FXlUUgxAXRApWHLo3KAe4DbQYCgFQhASYQgHw8RP5xlYzddh7VI2QTTiCAMxQIGFwAeTCQYUBs4EkRAA8FSQtUqAULQEngaShtXW9NPNV9QuKElVTNbIqgkLgLGAwAa1wAOQVAACJJFDhnPzgxJxARvQTmEBBkiRFkBUGF7IVJyRnZo3jkEEATIhAABloAC1xg7e2ZOXJhNX0t2dw2KBYr2eyQQHbSEHwMN4gADeTw+snkNREamk4LuKgq6lBVyKOkhynSPhAaw2qiqqPudVoHW6UgMKmQAFoAIyBBqPD4gAC+TWYbU6XSksGwKFQhgArCoUBFkFFYmFOTBuWhDAAiABieQsMtUQA)
  - [Stack ADT using an existential type](https://polytypic.github.io/f-omega-mu/#MQAgygLghgxg1iAggEQCogK4GcCWA7AcxCjxAFMAPHLCMvCHKAGxAgE8AHMgKG6bIitOZEAHsOggLwhA3cCBG4AB0AbTyi8ZAFwgAFAEoANCCyiAtppByAuiHy9+g9lxCAe4CbUIGgFQhASYQhv0vIK4oLacoZuNBa6Nnh2AkJOkLAIgTQpXr7+CgDe3CDkJhJsWoAARIrp8Bb6+SAc2AAWZYpyWZUIrX7t1bUc4s0K3Z1iEjrhRtBVcrrcAL6x8RCAUAztADrJ8IDQDJJLkRCreQVkRewg0oDZwIoqaiLSepY1BfVYDWcglwoyFBqKX1gaewsymMZje2gohgoWF0D164jeHz+APcQKhc02WkAwETaDZwGK2PgJKAAfW6klquIUx2KSnwEEstUA7aTgSZwBTPBq0+jWADMTJZKXZjS59JAAEZ+ZSOSLrABWBaEwTOCCiYl7DTlQasixZQGtC6-f5DBQU1ns8RKKxGEDMmBQLAiQ4FECqdRvGR6ZSu246GGPZ0gn0e77VEBQrRDXTA0w+8GGFVq9yW6xQmG1eYEhN7GXEUmsoA)
  - [Stream fusion](https://polytypic.github.io/f-omega-mu/#MQAgygLgTgpghgWxAMwK4GcCWB7AdgKH1ACE50YATEPECACxhAAc4mYoiiRvJZEUMOXAC4QAMSjYkAGUzoI6WtnDR4CRRGUA5bPUy4A5iDgQQAQQA2FrtwAiqXAGM4uEAGFsqCAoA0IAEpSLiDSMOiOdProANaYAJ5+tjSQMADucFAQNiB03kzCAPQFjpgQYTDsAHRyEJVM6KiVlKgFAG6YaRTYjgUNCAgZcQD8XZgAvACMAAyVE7OzUwAslQDsi1MThBYwphBxbNRMpmMggN3AgI3AlQDauHgwogAUAJR+6FL3IOcAuiD6hCRkTCOECwRyoKBYVqMCw1Wj7GD4ba7eEgQA9wDD5MIAFQgQBJhCAcScLpVsEcQA9zn4MaZzk9fgRETsQrCxvhuEj6ch9KUYAB9bDIEAnQDZwJdTgAPYSXVHi9DCamfa5vBCME4PcV+WVPH5-dlM1EQODRPkiQAARJd9KZ8QrzniQDahSBRZVTiJLS7ZfLYZc2dxfoLXIATIhAU1oDAIfr9NzujueX193BgFnICb9spAgHbSEDOcggADeqcjt1wqrOz2uxdLcZ8hb9ytLp3VUs1cptTyV71jGtoRpNV2+5NwAFoJnStfHI9wAL7030cnCGx2u4Tu1Mc9HYUkr3BW+2wy1LzDb2q1zCCzAgAA8IFc9Bgriu9djmCpm6Y5IvAGoQKPtSAk7m0YlrGf66n6FhviGvpgXm+hcrgPL8sgfiGsavK4H4C5wDOfxcDoriguCkKMDA4o1PeECYHAFggPIfBIHsbCMhAgBQDHRagADq8GogDQDKyerIgc8gwO+RJiug1xdCWjwvCAcQdBYFCPJStGyTEmD5LROoRvaTKMYw7H8GJlSAMBEEkPOgrx2sJ762ugdJgRyAq8gqIpip6NqVCxNocQ8qayjWkY5owBaTjeMZEuWVxSVW2qBZOT5Eo4wgUlS3rtlc8lJlQJyOBOfpTr6Tw8aIhlILajl6dgLksk6Ypsb5AU3qREDFcIZWKmuVU1fIjoGtV1LtXankSnKEm1iW4qmOmWbBfmtbcDFS5RZWIFfPFYVZYpy10EpnwoRYpUZYl5K7ShA2whAFhxQttGxKJSg9VkYU4Tp3CaE9IDppVpgDA9zrCoATcAusgIi2viwOnA16qWc1U1tR1lzQ7WAW1qN7WVLdk3TYos1kCFt2LRFZYZUtao3WFkZbTlZa7aIKlXUd1zU7GoM5BQB0U5T3DqQ9py8xjj73flL2pm1tGqPwgOzgJKCYBYZRQI6zqnPk4MgAARpuFgutDTXYwjkvlV56C+Sjllo5643c9jX249m+PzdzRPAZFpPE9WhNyQpNONnT+20IdqmY87fpnsw7NhveICZT7sZnYHf4AYwQsaY6V3rV7vNLgLEmp0wIuToVkbi4jMu6aYrBsLgNMq2xEyNegEx+Njo4lYjuvoAATI3Xcty1XeG-RnWRhy+m0Y6VxgBMpXN+AXelV32nI2FU8TI6soTJnYVzaFlPTznM9N5Ure0eveO5nv3NkyTSr3ZPYBdxv3dfNvzss5F-sM0HTcZR-p0cxjgfE4Gcubc2zkSJu7U5j5wfuvE4TdX63SnBtScj8c4L27ifAetEn4XwJqHG+jZ3au3JGAym-8-Z7W-ovP+cc1QJynk-EBFgvjkLChAs43d2pdzvmnE4TDHQv0LpGYuRcxYlQlsPCqOk2JGw4nmZy1JzpPT8H9PwXIFbsD8FXe8FApySLMkbbEdosSVCvtQZAT1RDmkqA6fEiNUEfUGiAWxZdrTelQX9GxlxTTAwpHaQGdIHFG0+NZUJgNUGaMVj4yoAT8Ra2wNdcJ0iUlqH2qmXRNdYnuKkek9WiNfSvX+CAAAouKRATBtggAwHAAwCIOTphOLIeQ1RdBwBAAAVlnNxRAlQskUCuJaH4DxekIBJFY6kQydw-C1OSMZEynrTIgLM+y+AswLL+ssr42zyQSmPJUcUIBvyjnWSoeilRonsF2Q8fZ7ojkAFIQDMN2acjZRtKhbOGTcu5O5DkEmeU8M5CznE1G2UAA)
  - [First-class type equality witnesses](https://polytypic.github.io/f-omega-mu/#MQAgYglgTgzgLgWgMYBsCGMYjgTwA4CmIBAjgK5ooS4gDu1AdgZswFCsoFzb5EAyBCACMGEAF4gAvCEDdwIEbgAHQzATcALAAESAx4ABcAKhCAkwhA6FGkHIMhTykBAYcuPQuAD2KACZTZileu17D-kYKABRqgM3AJiBhUQCUFqbm1rbsnNy4TgCiJJ4ypLoWgcYA3qwgIM4AZmCublogaopqqtXuZiDWhqRtygA0peUVAsKiYnUN6qpDIuLdFl2JfWVQBBUoY4rzZosgMDgAtnvrEwqbHcTZ1nLbcFBoDDBHTeoRXdbRndmJc59R186PqlOFnMhl6-QqUGch3qjUBPzOZzkrAAvjY7PZuFMRlJ+qkBi0PNJANnAiiJqhkaCEWgJ3QURL8BSUAAstAkFJSANryJSqBLxdoAXRAwSJERkAA9WVEFOK4ky0biHJUsTNiaShEpKVoVRJzBrKQqynjlqtPCS6QyAkpJWzxYaQHjdgczaTyVqdbT6fkrZyfFZ+XIhcEJVLFLL7XibncsGq6apRUohEhtYJphI3pqqR7EhbvUEQ2ykxyNEHOSWQOHkkaHHBnC645mU8MZjnfeKZUKqw6HBCofWyY3s+0FLWOcoBRzA8KnXtJxPxyBKXEu4AoBh1AB0ipUCT0BjrdyaULuZ7uo-dT85d729sjANAMdUAwETBLLL9F4mmSRXcbc1ftuqk0q2QgRkqgyptisYDhSWbgS2w5eoUjIyCy4SRNEERlmYQZMnOr5fiAh79paRhIRUWioaY6EKBUc4gdwM5-o2gHwcRhRKGRoQRJRsTskIXLeLy7RYSAFR0dgtz3IxCYyEm1K-hm0Fya0OZEoAq8B5mx-RlDIZGoapUTSkWMhioALcAKPp5jvCAFkgCZQbSZKNkROGwScqpQoVHh1ZpHWkH-kpHhAfxPIKCCgrCg5WjOV53bcNejGKcxqijuOtHBDOc5jkKS72iuBKbj+7i7sqsFiAeKxHjs+x7KeEkwBeV6Qje94gE+L4KqwRT7i47jIkAA)
  - [Encoding of a HOAS GADT](https://polytypic.github.io/f-omega-mu/#MQAgogdgxg9gJgSwgcxDAZiAhiAEgeQEEBlEAcUIBEAVAKFoBsBTAFxBYE8AHJkQHuAwADy4AnAFwAqEICTCEFIC8IQN3AgRuAAdIAAiQIPAkmXLUBhLAGcmxkFr2WV9LBDj8jp42IAUU2RICUehcp0e+gDetCAgAGpYDGIgGuphCYlJySkpKlYgKgA0oSAAQkgxcZqATcCagM3Aaq7psiV65T6yQqKZes0iIHWyluU5CQCS6EXxKe0gAEYwMAxtwh014HOt3Zl9IIRcXMOlaqkJY9V6JY2LLQuWJWsAMlgAttsaZXvJrmMLY8cZh7VeawBiCEEDyeewO306JwWnxW4OOtAAviAkPRmGxIjNFIBs4BGiSUQJxz0JiUxAV8aiUUBcTjMFjUlLU6IA2ioALogQRIiCMVj5JAgLHqTFlTGVJRDKH1clAt7kjhiD5qEm6TzkylianmLR04xqAoQZksxklQ3lNmYDkcTnctggQb8kDY3ZJCnypaTabklius7kpje+aK0kqilUkw0rX0wYGkBQdggXjI1HrTb2x1Cp3KIYHKFeKX+zIZ4lB-Qh9VhzXatQbLgGo1m9lWpM3W6pwUgzP595LduEpWBCSq0POWn05u140gTCJnkAjkCxXtsX51ywyFHXMpPtk0sakc62fj+vI60gW5YFhQAAWqeLA535dpjqUfplTEZWjZlKtJ74TAAblEYjFG8ejpIoZ4XpejKqGorIgCEYToq2ebqIIax6sh6YilKHAAF5iBKsiitKSzqEocoKoIeEgK4-5RNGgg+LRAEMHWIAcL8uS2pg84umM7oMJ6na+sJdGsXBrgIJgYmMgJn7sJeTAQHGTAMKY8acWE1aYWUeIUUsq55q+LHMkcbJUTRMlwYx1x3Dp5JZqRswtLp+JqDJE6uJgrhMtZXiaSAs72UuYI5qB65qFxIBJjx8Ymau4W1Eeyl8MgBFHChaiYKgggIt+SZ-FgUBsIoB5ICwhrlWyri5GO5V6FVjL1bIVU0SFBnNZyLAbnVEAVU1fXVXi+blZFiRRq1rh6gN-WNXJbVAqN5FiKNc4zWyHENj59GtQADP5UVhNtrGtQAjF4h00dNjU3YNC0rX1soPSwaiCBIHGXWEuWpK41bretk40Z9CTXYN-2TcNS1yqtAC0H1Et9hLHQD53+WjPjHjJk1-bdFWBUVbDI61ACs-lAA)
  - [Type-indexed trie](https://polytypic.github.io/f-omega-mu/#MQAgKgngDgpgtASwHYBMYA8YpAFwE4IwBQRANjDrtDCAPZSUC8Ig3cCCNwAHQDaStSMALhAAKAJQAaEAGdaAW0Eg2AXRDIS5SjmogAhqSatOLQE3A3AJJIAjELaSLAJiHGVashSqwQgHuAwBQQCoQQCTCEECQwOYWQC7gDhZAXuAOQAAiQEHgATDQ4NCOAGEdKRgpEGTM4qiQOJIdVG9c-KkBYXSm0UyI1lSm1o4AbyIQEABVJAQcIUSE-smp6Zmp+ko4kpEW2f648T6QAEF9EDGExKjLJKj7Dl9CECPyzIuaU5uQ4uE9SmvTlvXNgAUdBDx9idjoczv07ldLCJwQ84i0niIjpIPlMKgBfVRIdTuIYjEDMQDZwBNZiwAG4CeblDiran9fEAzrhWIAY3qtQKIDwHBZHBxOC4cRUJIxbkoO0oeJAhI4+KO0tOsRw1mhkISLBwjmh9kpdLSrS6LBZAjZhU53LF-KUXCOltOKkVuC1ag0IF+-wlUplxxlZzVAmVUL8Vy1sKpUx1DKyBtZeXZpqkHFdeAtVssNvsduFIpAsh0OCZAAt3R09Yyo0aY4VknLparRtDKXzkioWZnnV5SLRaABrACuUAEhySCXrixCZRCFMWzBzefzXGisQS47oDHKKl6-V57tVZMnsTEHBJG36Yv6BISnrlPsVfsD11rGrvwa5MZAG+mFkhkS71llHe7fYppaAq4JCP7Hh+SBat+jjyv+vZQFa6YWg6IBdvYmz9KiEGJtucpevKvr+sImrlKI+4-gIiJobB9jkZhIDwYB1pcKRIGUD+ICAO2kIBMq+75TLw-ASiwB48HwNDMGISgQVMMjyCJdZPpSTGIbaKEcRhUyokQ6KuGIQA)
  - [GADT type encoding using type equality witnesses](https://polytypic.github.io/f-omega-mu/#MQAg4gggIgKiAuBPADgUxKgdgYwPYBMBLTAcxAFcBnYspNDAR3IEMAbQpEAdw81Usr8AUENap4CFOgAyqQgCNMhAF4gAvCEDdwIEbgAHSbATcC7AAESAx4ABcAKhCAkwhCXdpkNtsgnBkMRFiJddAFEGdS1UBitXazsHAG8hEBAAJ1QAM1ZzEGM9UOdnABo4kEpEAFti9MyTI2yXDztsj218+PgE5kxKcr1jI2NAZuBdepBe12qQWsYc3qaEXE7KgaCa1xc7A2nkhNwyjK6qxbHXce0hAF9PTG9xQCgGUIAdQMBoBjUCn0SU1mDAbOA9T4tI+30AA9zE49IDzi9xIUSsUvj8jJpmPJzLIFEpVDVdL9wlFdEiANqGRwHOyggC6IAAFJpgaDdICAJQQ+KvFptShwrFGT79TTybAouSKFQHXlIwVokWY7H-Bw0kHOXT8-GmCmUgmqkCM5kgVm4TmfBHi1HCjFjPHyQmA+kUrwsqEbLYGo3Ik3onJGeC4fEGMn47Rqoqlf1+30gJFMu0gK5ulS3aJJVK5aGlZNs9pp3DJx3FE4PdKAYCJKYFIxdRFC-OHWBINDp9EZ8QBJTAARnSjRAzYATOkw15WVJPJR9bW9ETKSsxsmjqXLr5B4Ae4BgUhx9mCmkAI8C6fEFABCuFwaRA+8PIA302b8HSl7P0wAyi0aOkHwkaLeChBq4W9AWjJ+JBuHrTAACswhAJN+ui-rooHgWeQEFI2w6QdBSH6oBNRCGSQhtPgIALieaSyuuW7ZPIB4fBuOGYHhC6Xqu1i1qRQTEAB1G0S+T7EUxCyFI+pBnux+H-gxERiTxdbjtk6rVh6TLniAy70B2SnoAYDJCQusEQcRUTiVoW6SUYlLSR26m3opg4qYO6maWhol6XpEljsZowKYQw5AZZ9C2V45YSIRwT4oFGiBLoiasPi5GHmStoXK8N4aE2mA1iAYURfirGxRCrycQJSV5WQoUMOF7z4pQ-EkNl-ZQv+XzMFi8j6Mw5iqeG+jIm1TUFPidVqFczC3Fc8i3MWJUZWwEjMCA8hksmzDJvIDL5iARZ1TJU0zUyzDLShf6yRt4ZbR6cX+SA2n1Y1zWtYODV8jd9DdfE+IXf1g3DaN6Vleqi0MnN4a-StRYXT9x07UDP5GCDC3HTUp0JZ5GifA1nxNYijbIh5+rTWjnWDk9IBNojA1DSNY2lak+LMP9zAY4DhaUmhR3g3tuhM9NcMQmdFWvqQAD6Xp8wAVsOmDBPAzAJCQ4jlZVrg8zQFIAEQAFJ3gA8gAcroCukIQySIErOVQqxAu4HzutFQgkvS-AmUpfLlXK+CagAHwgLcSuewA1ICRs1RIlvC7gxDizbMuW47vNkHYlvO+o7uIAnWogN7ICG1zrwLoLlvmMYW5tYBdhF3x0dfFu2DMIIICxPEIUhGEgWGcCW4FPE+tpSVOZRRRfobhS2TgvAAAWWAgErLTkKgRuoKw1dK8kbCCErF4O7WoTmDezfmK38TtylZsW3L5Pd1l+L9xMjL3nL69hIVZ5Ajvuht6XNCHyLuBiyfmzFLL0d9wPIIV8PyyVrCTD6JkGBphhvAJaDwIZQX2gBD0z8966grObIO00NA50qlTCkEsdT2l8JgyqfN5Di1IdHKKBCKFRniJoK8W4wrd0mkdWa59AEIBAIAdtIQCV2rrXNBnZWzrhag1IOQsQ5f0kdI8eyUWzBCNpSXB0c+bTR2kyJWZIV4v3iN2dcyImqyNDpSExYslbJS7Eoqkqi34UKWlonRL8TggTAgkdc4CyahGgWmOBCDoIXQwuaF+eoj5qOwTMcJNB8HWyIegkh0T+YUJwVQmJs0EB0IuGgxhT9XigyWsEFhP98QFL+pwiYEh6F73MVSPRNSyFSNMfUtBtTKQtOEbUpWlJlF2P5hohkGlhHDPHsmJWQyRnxBUWk5JW0JnDPGbo-RiMtBeNGj4qkzAuzyGTHzBkfjloBKMEzYJRhQkYKSSQchlDLk0MyfE3JW4+lXIoeqbZVJilbHwRU7I8BBmnC5s80p0VWDzVkpHVif0CiUm0j3GKVNwVyyyseCiVJ-x-0VvbeAFJ76Xn+VMxec9UDJgUUo5IB5tEMiAA)
- See the [syntax summary](SYNTAX.md).
- See project [TODO](TODO.md) list.

## Why?

[Typed λ-calculi](https://en.wikipedia.org/wiki/Typed_lambda_calculus), and
[System F](https://en.wikipedia.org/wiki/System_F) in particular, are popular
elaboration targets for programming languages. Here are a couple of papers using
such an approach:

<blockquote>
  <dl>
    <dt><a href="https://people.mpi-sws.org/~rossberg/1ml/">1ML — core and modules united</a></dt>
    <dd>Andreas Rossberg</dd>
  </dl>
  <dl>
    <dt><a href="https://arxiv.org/abs/1206.5386">Elaborating Intersection and Union Types</a>
    <dd>Jana Dunfield</dd>
  </dl>
</blockquote>

Perhaps a practical System Fωμ implementation could serve as a reusable building
block or as a forkable boilerplate when exploring such programming language
designs.

System Fω might also be a good language for teaching functional programming:

<blockquote>
  <dl>
    <dt><a href="https://dl.acm.org/doi/abs/10.1145/3342713">Lambda: the ultimate sublanguage (experience report)</a></dt>
    <dd>Jeremy Yallop, Leo White<br><a href="https://github.com/ocamllabs/fomega">System Fω interpreter for use in Advanced Functional Programming course</a><dd>
  </dl>
</blockquote>
