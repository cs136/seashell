package main
import (
  "bufio"
  "fmt"
  "os"
  "os/exec"
  "github.com/urfave/cli"
)

func runscript(scriptname string) *exec.Cmd {
	//Code by Nathan Leclaire (https://nathanleclaire.com/blog/2014/12/29/shelled-out-commands-in-golang/)
	cmd := exec.Command(scriptname)
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(cmdReader)
	go func() {
		for scanner.Scan() {
			fmt.Printf("%s\n", scanner.Text())
		}
	}()

	err = cmd.Start()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error starting Cmd", err)
		os.Exit(1)
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error waiting for Cmd", err)
		os.Exit(1)
	}
	return nil
}
func main() {
  app := cli.NewApp()

  app.Commands = []cli.Command{
    {
      Name:    "cmake",
      Usage:   "Cmake the package to '_build'",
      Action:  func(c *cli.Context) error {
	runscript("./build_tools/cmake.sh")        
        return nil
      },
    },
    {
      Name:    "install",
      Usage:   "Runs 'make install' to '_install' and sets symlinks",
      Action:  func(c *cli.Context) error {
        runscript("./build_tools/make_install.sh")
        return nil
      },
    },
    {
      Name:        "template",
      Aliases:     []string{"t"},
      Usage:       "options for task templates",
      Subcommands: []cli.Command{
        {
          Name:  "add",
          Usage: "add a new template",
          Action: func(c *cli.Context) error {
            fmt.Println("new task template: ", c.Args().First())
            return nil
          },
        },
        {
          Name:  "remove",
          Usage: "remove an existing template",
          Action: func(c *cli.Context) error {
            fmt.Println("removed task template: ", c.Args().First())
            return nil
          },
        },
      },
    },
  }

  app.Run(os.Args)
}
