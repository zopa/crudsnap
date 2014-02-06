Todos and questions
===================

Groundhog
---------

  - Groundhog-th doesn't seem to like lens-style leading-underscore names
    (probably because it needs to upcase them). It'd be nice if the
    quasiquoter knew to strip leading-underscores.

General architecture
--------------------

  - I like the approach of treating these like formlets -- expressions
    built up piece-by-piece that we can interpret in multiple contexts.
    But it's not exactly the same problem, because, whereas forms are
    composable (each field is a bit of HTML we can concatinate), reading
    a field from a database only makes sense in the context of the whole
    record. I'm working around this by using a lens to project from the
    results of a database read, with the read action supplied as state.
    But it feels awkward, and could easily end up with one database read
    per field, which is dumb. Is there a better approach?

    Monadic validation in formlets has the same issue, no? What can I
    learn from that?

  - I believe --- although I won't swear to it --- that Compiled Heist
    requires that all the splices be bound at loadtime. It's not yet 
    entirely clear to me whether that's possible with the architecture
    I have. It'd be a pity if we needed to use interpreted splices.

  - Should render a success page after inserting a record.
    But we should make sure it's *actually* succeeded --- lying is bad.
