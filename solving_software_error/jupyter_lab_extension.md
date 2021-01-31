# Jupyterlab extension

1. **Install Jupyterlab extension**

   Enter the following command in the terminal.
  
   **jupyter labextension install 'name of jupyterlab extension'**
  
   ex) jupyter labextension install @jupyterlab/google-drive



1. **Check install**

   Enter the following command in the terminal.
  
   **jupyter labextension list**


  
1. **When jupyterlab extension is not working**

   **Removing the 'jupyter/lab/settings/build_config.json'** file worked for me.
  
   Enter the following command in the terminal to find out the exact path.
  
   jupyter lab path
  
