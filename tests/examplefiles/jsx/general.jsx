const isOldEnough = (value, ownProps) => {
    if (parseInt(value, 10) < 14) {
        return "Only 14yo and older can register to the site."
    }
};

// functional component
const BlogTitle = ({ children }) => (
  <h3>{children}</h3>
);

// class component
class BlogPost extends React.Component {
  renderTitle(title) {
    return <BlogTitle>{title}</BlogTitle>
  };
  render() {
    return (
    <div className="blog-body">
      {this.renderTitle(this.props.title)}
      <p>{this.props.body}</p>
      <CustomComponent>text</CustomComponent>
      <input type="text" {...props.inputProps} />
      <button aria-label="Submit">Submit</button>
    </div>
    );
  }
}

const body = "Hello World!";
const blogNode = <BlogPost title="What's going on?" body={body} />;
// some comment. Tags shouldn't be lexed in here
// <div class="blog-body">
// <h3>What's going on?</h3>
// <p>Hello World!</p>
// </div>

/*
  Some comment. Tags shouldn't be lexed in here either
  <div class="blog-body">
  <h3>What's going on?</h3>
  <p>Hello World!</p>
  </div>
*/

const shortSyntaxfragmentEmptyBody = <></>;

const shortSyntaxfragmentFullBody = <><div/></>;

const reactDotFragment = <React.Fragment><div/></React.Fragment>;

const reactDotContext = <Context.Provider><div/></Context.Provider>;

const reactDotContextValue = <Context.Provider value="Hello"><div/></Context.Provider>;
