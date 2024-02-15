## Linux/WSL Installation:

To install and run the project on Linux or Windows Subsystem for Linux (WSL), follow these steps:

1. Update package lists and install Node.js and npm:
    ```bash
    sudo apt update
    sudo apt install nodejs npm
    ```

2. Clone the repository:
    ```bash
    git clone https://github.com/luis-aparicio/nfl-game-possession-tracker
    ```

3. Install Docker. Instructions can be found [here](https://docs.docker.com/get-docker/).

4. Navigate to the repository:
    ```bash
    cd nfl-game-possession-tracker
    ```

5. Build the Docker container:
    ```bash
    docker build ./.devcontainer/
    ```

6. Run the Docker container (replace `{image_id}` with the image ID obtained from the previous step):
    ```bash
    docker run {image_id}
    ```

7. Check for running Docker containers:
    ```bash
    docker ps
    ```
8. Check for ended Docker containers:
   ```bash
   docker ps -a
   ```

(Alternative) Use Visual Studio Code to run the devcontainer.
