const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

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

const hubCfg = {
  ...shared,

  entry: "./src/hub.js",

  plugins: [
    new HtmlWebpackPlugin({
      favicon: "./static/favicon.ico",
      template: "./src/hub.ejs",
      inject: "body",
      filename: path.resolve(__dirname, "dist/hub/index.html"),
      publicPath: "/static/",
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/hub/static"),
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
      filename: path.resolve(__dirname, "dist/ucm/index.html"),
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/ucm/static"),
    clean: true,
  },
};

module.exports = [hubCfg, ucmCfg];
