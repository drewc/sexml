(defpackage :sexpml/html5
  (:use :cl)
  (:nicknames :html5)
  (:import-from :sexpml
		#:<>
		#:sexpml-form)
  (:export #:doctype 
	   #:html
           #:head
           #:title
	   #:body
	   #:script
	   #:div
	   #:nav
	   #:iframe)
  
  (:documentation "HTML5 tags for SEXPML 
 - Symbolic Expression eXtensible Programmable Markup Language -
 - http://www.w3.org/TR/html51/"))
(in-package :sexpml/html5)

(defmethod sexpml-form ((tag (eql 'doctype))
		     &key attributes contents)
  "=> /form/ 

*Description:*

A DOCTYPE is a required preamble.

DOCTYPEs are required for legacy reasons. When omitted, browsers tend
to use a different rendering mode that is incompatible with some
specifications. Including the DOCTYPE in a document ensures that the
browser makes a best-effort attempt at following the relevant
specifications.

A DOCTYPE must consist of [...]. In other words, <!DOCTYPE html>,
case-insensitively.

It is suggested that newlines be inserted after the DOCTYPE [...]."
  (declare (ignore attributes contents))
  `(<> :unescaped 
     "<!DOCTYPE html>" (string #\Newline)))

(defun sexpml-form-with-end-tag (name attributes contents)
  `(<> '(,name ,@attributes)
     ,@(if contents 
	   contents
	   (list ""))))

(defmethod sexpml-form ((tag (eql 'html))
		     &key attributes contents)
  "http://www.w3.org/TR/html51/semantics.html#the-html-element"
  (sexpml:sexpml-attributes-bind ((doctype nil)) attributes
    (let ((dt (gensym)))
      `(progn ,@ (when doctype
		   `((let ((,dt ,doctype))
		       (when ,dt
			 (<> '(doctype))))))
	 ,(sexpml-form "html" 
		    :attrbutes attributes
		    :contents contents)))))

(defmethod sexpml-form ((tag (eql 'head))
		     &key attributes contents)
  (sexpml-form "head" 
	    :attributes attributes
	    :contents contents))

(defmethod sexpml-form ((tag (eql 'title))
		     &key attributes contents)
  "Content model:
Text that is not inter-element whitespace.
Tag omission in text/html:
Neither tag is omissible."

  (sexpml-form-with-end-tag 
   "title" attributes 
   (if (every 'stringp contents)
       (list (sexpml-form :text :contents contents))
       contents)))

(defmethod sexpml-form ((tag (eql 'body))
		     &key attributes contents)
  "http://www.w3.org/TR/2014/WD-html51-20140617/sections.html#the-body-element"
  (sexpml-form "body" 
	    :attributes attributes
	    :contents contents))

(defmethod sexpml-form ((tag (eql 'div))
		     &key attributes contents)
  "=> /form/

*Description:*

The div element has no special meaning at all. It represents its
children. It can be used with the class, lang, and title attributes to
mark up semantics common to a group of consecutive elements. 
 -- http://www.w3.org/TR/html51/grouping-content.html#the-div-element

*Tag omission in text/html:*

Neither tag is omissible.
"

  (sexpml-form "div" 
	    :attributes attributes
	    :contents (or contents (list ""))))
 

(defmethod sexpml-form ((tag (eql 'iframe))
		     &key attributes contents)
  "Tag omission in text/html:
Neither tag is omissible.

The HTML parser treats markup inside iframe elements as text.
-- http://www.w3.org/TR/html51/semantics.html#iframe-content-model
"

  (sexpml-form-with-end-tag "iframe" attributes contents))

(defmethod sexpml-form ((tag (eql 'script))
		     &key attributes contents)
    "=> /form/

http://www.w3.org/TR/html51/scripting-1.html#the-script-element

*Description*

 The script element allows authors to include dynamic script and data
 blocks in their documents. The element does not represent content for
 the user.

*Content model:*

 If there is no src attribute, depends on the value of the type
 attribute, but must match script content restrictions.

 If there is a src attribute, the element must be either empty or
 contain only script documentation that also matches script content
 restrictions.

*Content Restrictions:* 

#+BEGIN_QUOTE 
*Note:* The easiest and safest way to avoid the rather strange
 restrictions described in this section is to always escape \"<!--\" as
 \"<\!--\", \"<script\" as \"<\script\", and \"</script\" as
 \"<\/script\" when these sequences appear in literals in scripts
 (e.g. in strings, regular expressions, or comments), and to avoid
 writing code that uses such constructs in expressions. Doing so avoids
 the pitfalls that the restrictions in this section are prone to
 triggering: namely, that, for historical reasons, parsing of script
 blocks in HTML is a strange and exotic practice that acts
 unintuitively in the face of these sequences.

-- http://www.w3.org/TR/html51/scripting-1.html
#+END_QUOTE 

*Tag omission in text/html:*
  Neither tag is omissible"

  (let ((contents 
	 (if contents
	     (list (sexpml-form 
		    :unescaped
		    :contents contents))
	     ;; *Tag omission in text/html:*
	     ;; Neither tag is omissible
	     (list ""))))

  (sexpml-form 
   "script"
   :attributes attributes
   :contents contents)))


(defmethod sexpml-form ((tag (eql 'nav))
		     &key attributes contents)
  "Categories:
Flow content.
Sectioning content.
Palpable content.

Contexts in which this element can be used:
Where flow content is expected.

Content model:
Flow content, but with no main element descendants.

Tag omission in text/html:
Neither tag is omissible.

Content attributes:
Global attributes

Allowed ARIA role attribute values:
navigation role (default - do not set) or presentation.

Allowed ARIA State and Property Attributes:
Global aria-* attributes
Any aria-* attributes applicable to the allowed roles.

DOM interface:
Uses HTMLElement."
  (sexpml-form-with-end-tag "nav" attributes contents))

