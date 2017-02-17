const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
module.exports = {
  entry: [
    './src/index.tsx'
  ],
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'bundle.js', 
    publicPath: '/dist/',
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './src/index.html',
      filename: '../index.html',
    }),
      new CopyWebpackPlugin([
      {
        from: './node_modules/monaco-editor/min/vs',
        to: 'vs',
      }
    ])
  ],
  devtool: "source-map",
  resolve: {
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".scss"]
  },
  module: {
    preLoaders: [
      { test: /\.js$/, loader: "source-map-loader" }
    ],
    loaders: [
      { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
      {
        test: /\.scss$/,
        loaders: [
            'style?sourceMap',
            'css?modules&importLoaders=1&localIdentName=[path]___[name]__[local]___[hash:base64:5]',
            'sass?sourceMap'
        ]
      },{
      test: /\.css$/,
      loader: "style-loader!css-loader"
      }, {
        test: /\.(woff|woff2|ttf|eot)$/,
        loader: 'file-loader'
      },
      {
          test: /\.(jpe?g|png|gif|svg)$/i,
          loaders: [
              'file?hash=sha512&digest=hex&name=[hash].[ext]',
              'image-webpack?bypassOnDebug&optimizationLevel=7&interlaced=false'
          ]
      }
    ],
  },
};
