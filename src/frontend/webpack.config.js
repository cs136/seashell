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
    filename: 'bundle.[chunkhash].js',
    publicPath: '/',
  },
  plugins: [
    new CopyWebpackPlugin([
      {
        from: './node_modules/monaco-editor/min/vs',
        to: 'vs',
      },
      {
        from: 'favicon.ico'
      }
    ]),
    new HtmlWebpackPlugin ({
      inject: true,
      template: './src/index.html'
    }),
    new webpack.DefinePlugin({
      IS_BROWSER: true,
      PRODUCTION: false,
      LOGIN_URL: JSON.stringify("https://seashell-dev.student.cs.uwaterloo.ca/~cs136/seashell/cgi-bin/login2.cgi")
    })
  ],
  devtool: "source-map",
  resolve: {
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: [".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".scss"]
  },
  module: {
    rules: [
      { enforce: 'pre', test: /\.js$/, loader: "source-map-loader" },
      { enforce: 'pre', test: /\.tsx?$/, loader: "tslint-loader" },
      { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
      { test: /\.scss$/,
        use: [
          'style-loader?sourceMap',
          'css-loader?modules&importLoaders=1&localIdentName=[path]___[name]__[local]___[hash:base64:5]',
          'sass-loader?sourceMap'
        ]
      }, {
        test: /\.css$/,
        loader: "style-loader!css-loader"
      }, {
        test: /\.(woff|woff2|ttf|eot)$/,
        loader: 'file-loader'
      }, {
        test: /\.(jpe?g|png|gif|svg)$/i,
        loaders: [
          'file-loader?hash=sha512&digest=hex&name=[hash].[ext]', {
            loader: 'image-webpack-loader',
            query: {
              bypassOnDebug: true,
              gifsicle: {
                interlaced: false
              },
              pngquant: {
                optimizationLevel: 7
              }
            }
          }
        ]
      }
    ]
  },
};
