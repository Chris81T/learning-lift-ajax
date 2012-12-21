package code.snippet

import net.liftweb._
import http._
import js.{JsCmds, JsCmd, JE}
import JsCmds._
import util.Helpers._

import code.model._

object CreateArticle {

  def render = {
		var title = ""
		var content = ""

    "@title" #> SHtml.text(title, title = _) &
    "@content" #> SHtml.textarea(content, content = _) &
    "@create-button" #> SHtml.ajaxSubmit("Save", () => save(title, content))
	}

  private def save(title: String, content: String) : JsCmd = {
    println("save -> title=" + title + ", content=" + content)
    if (title.isEmpty || content.isEmpty) {
			S.error("both", "Both fields have to be filled.")
			Noop
		} else {
			println("add given article to articles list...")
			Articles.add(Article(Articles.generateId, title, content))
			Noop // FIXME SetHtml("article-rows", EditArticles.render)
		}
  }
}

object EditArticles {

	def render = {
		val articles = Articles.getArticlesList
		println("found articles-size=" + articles.size)

		if (!articles.isEmpty) {
			"#article-rows tr" #> articles.map(renderArticle(_)) &
			"#emptyNotice" #> ""
		}	else {
			"#article-table" #> ""
		}
	}

	/**
	 * Function should render a table row for given article. Also it will bind the article and the appropriate input
	 * fields
	 * @param article has to be rendered
	 */
	private def renderArticle(article: Article) = {
		var inputTitle = article.title
		var inputContent = article.content

		"tr [id]" #> article.id &
		"@id [value]" #> article.id &
		"@title" #>  SHtml.text(inputTitle, inputTitle = _) &
		"@content" #> SHtml.text(inputContent, inputContent = _) &
		"@edit-button" #> SHtml.ajaxSubmit("Edit", () => edit(article, inputTitle, inputContent)) &
		"@remove-button" #> SHtml.ajaxSubmit("Remove", () => remove(article))
	}

	private def edit(article: Article, inputTitle: String, inputContent: String) : JsCmd = {
		println("--> edit article=" + article + ", inputTitle=" + inputTitle + ", inputContent=" + inputContent)
// FIXME error: reassignment to val
//		article.title = inputTitle
//		article.content = inputContent
		Noop
	}

	/**
	 * First remove given article out of the articles container.
	 *
	 * @param article to be remove
	 * @return a JsCmd including a jQuery function to remove table row with given id -- see in the render statement the
	 *         set id into the tr element.
	 */
	private def remove(article: Article) : JsCmd = {
		println("--> remove article=" + article)
		Articles.remove(article)
		// TODO if articles list is empty, rerender the snippet to show alternative content
		JE.JsRaw("""$("#%s").remove()""".format(article.id)).cmd
	}

}