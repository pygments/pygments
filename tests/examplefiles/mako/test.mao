
## Single line comment
<!DOCTYPE html>
<%doc>
    This is the base template
</%doc>
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	<meta name=viewport content="width=device-width, initial-scale=1">
  	<title>
	  	% if page.meta.get ('title', None) and page.url != '/':
			/ ${page.url}
		% endif
	</title>
	<link type="text/css" rel="stylesheet" href="/style.css" />
</head>
<body>
	<div class="header-wrapper">
	    <header>
	        <nav>
	            <ul>
					% for item in site.data['menu']:
						<li>
						<a href="${item['url']}">
							% if 'image' in item:
							<img src="${item['image']}" height="12px">
							% endif
							${item['title']}
						</a>
						</li>
					% endfor
				</ul>
	    	</nav>
	    </header>
	</div>
	<div id="container">
		<section id="content">
		${self.body()}
		</section>
		<footer>
			<a href="#">Imprint</a>&nbsp;|&nbsp;<a href="#">Privacy policy</a>
		</footer>
    </div>
	<script type="text/javascript">	
	</script>
</body>
</html>
