"use server";
import { exec } from "child_process";
import { cwd } from "process";


async function getSolutionFromFile(fileName: String): Promise<String> {
  return new Promise((resolve, reject) => {
    exec("./out " + "./sudokus/" +fileName + '.txt', { cwd: cwd() }, (error, stdout, stderr) => {
      if (error) {
        reject(error);
      }
      if (stderr) {
        reject(stderr);
      }
      resolve(stdout);
    });
  });
}

export async function getSolution(fileName: String): Promise<{partialSuds: String[], techniques: String[]}> {
  const solution = await getSolutionFromFile(fileName);
  const solutionArray = solution.split("\\n");

  solutionArray.pop();

  const partialSuds = solutionArray.splice(1);

  const techniques = ['Not implemented'];

  return {partialSuds, techniques};
}


