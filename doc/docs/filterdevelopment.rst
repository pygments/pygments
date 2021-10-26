.. -*- mode: rst -*-

=====================
Write your own filter
=====================

.. versionadded:: 0.7

Writing own filters is very easy. All you have to do is to subclass
the `Filter` class and override the `filter` method. Additionally a
filter is instantiated with some keyword arguments you can use to
adjust the behavior of your filter.


Subclassing Filters
===================

As an example, we write a filter that converts all `Name.Function` tokens
to normal `Name` tokens to make the output less colorful.

.. sourcecode:: python

    from pygments.util import get_bool_opt
    from pygments.token import Name
    from pygments.filter import Filter

    class UncolorFilter(Filter):

        def __init__(self, **options):
            Filter.__init__(self, **options)
            self.class_too = get_bool_opt(options, 'classtoo')

        def filter(self, lexer, stream):
            for ttype, value in stream:
                if ttype is Name.Function or (self.class_too and
                                              ttype is Name.Class):
                    ttype = Name
                yield ttype, value

Some notes on the `lexer` argument: that can be quite confusing since it doesn't
need to be a lexer instance. If a filter was added by using the `add_filter()`
function of lexers, that lexer is registered for the filter. In that case
`lexer` will refer to the lexer that has registered the filter. It *can* be used
to access options passed to a lexer. Because it could be `None` you always have
to check for that case if you access it.


Using a decorator
=================

You can also use the `simplefilter` decorator from the `pygments.filter` module:

.. sourcecode:: python

    from pygments.util import get_bool_opt
    from pygments.token import Name
    from pygments.filter import simplefilter


    @simplefilter
    def uncolor(self, lexer, stream, options):
        class_too = get_bool_opt(options, 'classtoo')
        for ttype, value in stream:
            if ttype is Name.Function or (class_too and
                                          ttype is Name.Class):
                ttype = Name
            yield ttype, value


You can instantiate this filter by calling `uncolor(classtoo=True)`, the same
way that you would have instantiated the previous filter by calling
`UncolorFilter(classtoo=True)`. Indeed, The decorator automatically ensures that
`uncolor` is a class which subclasses an internal filter class. The class
`uncolo` uses the decorated function as a method for filtering.  (That's why
there is a `self` argument that you probably won't end up using in the method.)
