import pytest

from pygments.lexers.templates import JavascriptDjangoLexer, MasonLexer, \
    SqlJinjaLexer, VelocityLexer

from pygments.token import Comment


@pytest.fixture(scope="module")
def lexer():
    yield JavascriptDjangoLexer()

@pytest.fixture(scope='module')
def lexerMason():
    yield MasonLexer()

@pytest.fixture(scope='module')
def lexerVelocity():
    yield VelocityLexer()

@pytest.fixture(scope='module')
def lexerSqlJinja():
    yield SqlJinjaLexer()

def test_do_not_mistake_JSDoc_for_django_comment(lexer):
    """
    Test to make sure the lexer doesn't mistake
    {* ... *} to be a django comment
    """
    text = """/**
               * @param {*} cool
               */
              func = function(cool) {
              };

              /**
               * @param {*} stuff
               */
              fun = function(stuff) {
              };"""
    tokens = lexer.get_tokens(text)
    assert not any(t[0] == Comment for t in tokens)

def test_mason_unnamed_block(lexerMason):
    text = """
            <%class>
            has 'foo';
            has 'bar' => (required => 1);
            has 'baz' => (isa => 'Int', default => 17);
            </%class>
            """
    res = lexerMason.analyse_text(text)
    assert res == 1.0

def test_velocity_macro(lexerVelocity):
    text = """
            #macro(getBookListLink, $readingTrackerResult)
              $readingTrackerResult.getBookListLink()
            #end
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.26

def test_velocity_foreach(lexerVelocity):
    text = """
            <ul>
            #foreach( $product in $allProducts )
              <li>$product</li>
            #end
            </ul>
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.16

def test_velocity_if(lexerVelocity):
    text = """
            #if( $display )
              <strong>Velocity!</strong>
            #end
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.16

def test_velocity_reference(lexerVelocity):
    text = """
            Hello $name!  Welcome to Velocity!
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.01

def test_sql_jinja_dbt_ref(lexerSqlJinja):
    text = """
    {%- set payment_methods = ["bank_transfer", "credit_card", "gift_card"] -%}

    select
      order_id,
      {%- for payment_method in payment_methods %}
      sum(case when payment_method = '{{payment_method}}' then amount end) as {{payment_method}}_amount
      {%- if not loop.last %},{% endif -%}
      {% endfor %}
    from {{ ref('raw_payments') }}
    group by 1
    """
    res = lexerSqlJinja.analyse_text(text)
    assert res == 0.4

def test_sql_jinja_dbt_source(lexerSqlJinja):
    text = """
    {%- set payment_methods = ["bank_transfer", "credit_card", "gift_card"] -%}

    select
      order_id,
      {%- for payment_method in payment_methods %}
      sum(case when payment_method = '{{payment_method}}' then amount end) as {{payment_method}}_amount
      {%- if not loop.last %},{% endif -%}
      {% endfor %}
    from {{ source('payments_db', 'payments') }}
    group by 1
    """
    res = lexerSqlJinja.analyse_text(text)
    assert res == 0.25

def test_sql_jinja_dbt_macro(lexerSqlJinja):
    text = """
    {% macro cents_to_dollars(column_name, precision=2) %}
        ({{ column_name }} / 100)::numeric(16, {{ precision }})
    {% endmacro %}
    """
    res = lexerSqlJinja.analyse_text(text)
    assert res == 0.15
