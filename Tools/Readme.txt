
OPS Builder is currently built with NetBeans 6.9.1


To build OPS Builder you need to do the following in NetBeans:

- Open the "JarSearch" and "ConfigurationLib" projects from the \Libs directory

- Build the "JarSearch" and "ConfigurationLib" projects

- Open the "OPS IDL Builder NB" project from the \Tools directory

  In the open dialog, select this project as main project and 
  select that all required projects should be opened.

- Check properties for the "JarSearchWrapper" project

  In the Libraries category and Wrapped JARs tab, you may need to re-add the JarSearch.jar 

  In the API Versioning category and Public Packages list, jarsearch and jarsearch.utils should be checked

- Build the "OPS IDL Builder NB" project
