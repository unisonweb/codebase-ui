const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const FileManagerPlugin = require("filemanager-webpack-plugin");

const shared = {
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
      },
      {
        test: /\.(woff(2)?|ttf|eot)$/i,
        type: "asset/resource",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-asset-webpack-loader",
          },
          {
            loader: "elm-webpack-loader",
            options: {
              debug: false,
              cwd: __dirname,
            },
          },
        ],
      },
    ],
  },
};

const unisonShareCfg = {
  ...shared,

  entry: "./src/unisonShare.js",

  plugins: [
    new HtmlWebpackPlugin({
      favicon: "./static/favicon.ico",
      template: "./src/unisonShare.ejs",
      inject: "body",
      publicPath: "/static/",
      base: "/",
      filename: path.resolve(__dirname, "dist/unisonShare/index.html"),
    }),

    new FileManagerPlugin({
      events: {
        onEnd: {
          archive: [
            { source: "dist/unisonShare", destination: "dist/unisonShare.zip" },
          ],
        },
      },
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/unisonShare/static"),
    clean: true,
  },
};

const ucmCfg = {
  ...shared,

  entry: "./src/ucm.js",

  plugins: [
    new HtmlWebpackPlugin({
      favicon: "./static/favicon.ico",
      template: "./src/ucm.ejs",
      inject: "body",
      publicPath: "/static/",
      base: false, // set dynamically by grabbing the 2 first path segments in the url.
      filename: path.resolve(__dirname, "dist/ucm/index.html"),
    }),

    new FileManagerPlugin({
      events: {
        onEnd: {
          archive: [{ source: "dist/ucm", destination: "dist/ucm.zip" }],
        },
      },
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/ucm/static"),
    clean: true,
  },
};

module.exports = [unisonShareCfg, ucmCfg];
