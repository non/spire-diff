## spire-diff

This is a generic implementation of the
[Diff algorithm](http://en.wikipedia.org/wiki/Longest_common_subsequence_problem)
in Scala, using Spire's `Eq[A]` type class.

*(This is a very young project. Names and APIs are likely to change.)*

### Overview

Often, it would be convenient to be able to find the differences
between many kinds of sequences. The unix command `diff(1)` will show
you the differences between lines, but what if you are interested in
characters, or bytes? What if you have a sequence of integers, or a
more exotic type?

Enter *spire-diff*! Since it uses the `spire.algebra.Eq[A]` type
class, it can compare sequences of any type that Spire supports, or
any type for which you can define an instance of `Eq`.

In the future, I'd like to be able to support something richer than
`Eq`. For example, you could imagine getting more fine-grained output
than *matches/doesn't match* when comparing maps or sets.

### Examples

```scala
import spire.diff._
import spire.implicits._

val left  = Array(1,2,3,4,5)
val right = Array(1,1,2,3,5)
val delta = Diff.diff(left, right)

delta.display(left, right)
```

This program will display the following output:

```
= 1
> 1
= 2
= 3
< 4
= 5
```

The way to read this cryptic output is:

 * First, both contain `1`.
 * Then `1` appears only on the right.
 * Then both contain `2` and `3`.
 * Then `4` appears only on the left.
 * Then both contain `5` and are finished.

### Under the hood

You can get a sense of what's going on by running the following code:

```scala
delta.slices.foreach(println)
```

This produces some even more cryptic output:

```
SliceB(0,0,1)
SliceR(1,2)
SliceB(1,2,2)
SliceL(3,4)
SliceB(4,4,1)
```

Each of these is a `Slice` -- `Diff.diff(left, right)` produces a
`List[Slice]` which accounts for how every element in both `left` and
`right` will be handled in the diff.

The big idea is that the `Diff.diff` call is not copying the
underlying data from `left` or `right`. Rather, it just provides
instructions on the best "path" to take through both inputs to find
the most agreement between them. This is why the `delta.display` line
needed to take `(left, right)` as arguments -- otherwise it would have
no idea what the actual data was.

### Future Work

There are lots of nice, high-level methods that are lacking.

It would be good to consider supporting more collection types.

Fine-grained element-to-element differences would be nice to support.

Other things I'm not thinking of now.

### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](COPYING) file.

Copyright Erik Osheim, 2015.

### No Warranty

> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
> BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
> ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
> CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
> SOFTWARE.
