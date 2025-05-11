## [Sequential Rank Agreement (SRA)](https://arxiv.org/abs/1508.06803)

### Interpreting SRA Values [sra(d) | d <- [1..(length options)]]:

Low SRA Value (Closer to 0): This indicates high agreement. A low standard
deviation means that for the items considered up to depth d, the different
lists are assigning very similar ranks to those items.

High SRA Value: This indicates low agreement or disagreement. A high standard
deviation means that for the items considered up to depth d, the ranks assigned
by different lists are widely spread out. The lists disagree significantly on
the positioning of these items.

SRA Value = 0: This means perfect agreement for all items considered up to
depth d. Every list assigned the exact same rank to every item within that
top-d cumulative set.

Intuitive Meaning: sra(d) can be thought of as the standard deviation in ranks
for the items found within the top d of any list. If sra(5) = 10, it suggests
that for items appearing in the top 5 of at least one list, their ranks across
all the lists typically have a standard deviation of about 10 positions.

### Interpreting SRA Curves [(d, sra(d)) | d <- [1..(length options)]]:

#### Trend:

Starts Low, Stays Low/Flat: High agreement, especially at the top, and this
agreement persists for lower ranks. The lists are generally consistent.

Starts Low, Increases: This is common. It indicates good agreement on the
top-ranked items, but the lists start to diverge as you consider items ranked
lower down. The point where it starts increasing noticeably is important – it
might signify a drop-off in consensus.

Starts High, Stays High/Flat: Poor agreement throughout the lists.

Starts High, Decreases: This is unusual for typical rankings but can happen
(like the Ridge example in the paper). It often signals an artifact of the
ranking method itself, related to how ties or items with very similar scores
are handled, leading to instability within a specific rank range that gets
averaged out later.

#### Elbows:

An elbow where a flat or slowly rising curve suddenly starts rising much faster
indicates a depth d beyond which the agreement deteriorates markedly. This
could be a potential cutoff point if you need to select a subset of items based
on consensus.

# LICENSE

Copyright (c) 2025 coriocactus

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
