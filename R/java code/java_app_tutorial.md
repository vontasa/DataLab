# Java web application for CPLEX#
This is a short check list for major steps to start developing Java CPLEX application. To master the web application development, the knowledge of following technologies is highly desired:
- Object-oriented Java programming
- HTML, CSS, Javascript
- SQL query and JDBC connection
- Basic CPLEX modeling

## Setup Java Enviroment ##
- Download and install lastest JDK and JRE
- Download and install [Netbeans](https://netbeans.org/downloads/) which is a free multi-language IDE. Make sure you download the version **all**
- Download [Apache Tomcat server](http://tomcat.apache.org/). Extract the file to the desired location for your web server.

## Setup CPLEX Enviroment ##
- Install CPLEX
- Add CPLEX library into system path
 1. Right click *Computer* -> *Properties* -> *Advanced system settings* -> * Enviroment Variables* -> *System variables*.
 - Looking for the variable *Path* and edit
 - Appending the following directories into the string. (Replaced by your CPLEX location)
    * C:\Program Files\IBM\ILOG\CPLEX_Studio1261\cplex\bin\x64_win64\;
    * C:\Program Files\IBM\ILOG\CPLEX_Studio1261\opl\bin\x64_win64\;
- Add **oplall.jar** into project library
- Reboot computer to take effect

## Basic JSF ##
JSF is short for Java Server Face which is a Java specification for building component-based user interface.
### Build New Project ###
1. Open Netbeans. Go to *file*->*New Project*->*Java Web*->*Web Application*
- Give a name to project
- Choose Apache Tomcat as the server
- Choose JavaServer Faces as the framwork

### Use Primefaces ###
Primefaces is an open source framework for JSF. There are over 100 different UI components along with ready-to-run sample codes.
Go to [PrimeFaces showcase](http://www.primefaces.org/showcase/) is the best way to learn them.
Our projects are built based on PrimeFaces 4.0. Some components have problem in 5.0.
1. Download [PrimeFaces 4.0](http://www.primefaces.org/downloads)
- Add jar file into project as external library.
- Declare the p tag in xhtml. For example:

```html
<html xmlns="http://www.w3.org/1999/xhtml"
    xmlns:ui="http://java.sun.com/jsf/facelets"
    xmlns:h="http://java.sun.com/jsf/html"
    xmlns:p="http://primefaces.org/ui"
    xmlns:f="http://java.sun.com/jsf/core">
```

### Hello World example###
index.xhtml
```html
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"
	xmlns:h="http://java.sun.com/jsf/html"
	xmlns:f="http://java.sun.com/jsf/core"
	xmlns:ui="http://java.sun.com/jsf/facelets"
	xmlns:p="http://primefaces.org/ui">

<h:head>
</h:head>
<h:body>
	<h1>Hello World PrimeFaces</h1>

	<h:form>
	   <p:editor value="#{editor.value}" />
	</h:form>

</h:body>
</html>
```
Editor.java
```java
package helloWorld;

import javax.faces.bean.ManagedBean;

@ManagedBean
@SessionScoped
public class EditorBean {

	private String value = "This editor is provided by PrimeFaces";

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
```
#### Keynote ####
- DO NOT put any character before the xml declaration in xhtml page
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
```
- Make sure Java, CPLEX are all either x64 or x32
- Use Tomcat server
- Use Primefaces 4.0 for running the old application
- Double check the system path for CPLEX
- Any java file change need to **deploy**(compile) before taking effect
- Make sure you using right scope declaration for java bean
