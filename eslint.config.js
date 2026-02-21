import parserTypescript from '@typescript-eslint/parser';
import pluginTypescript from '@typescript-eslint/eslint-plugin';
import pluginCheckFile from 'eslint-plugin-check-file';
import prettier from 'eslint-config-prettier';

export default [
  {
    ignores: ['**/node_modules/**', '**/dist/**', '**/build/**'],

    files: ['**/*.{ts,tsx,js,jsx}'],

    languageOptions: {
      parser: parserTypescript,
      parserOptions: {
        ecmaVersion: 'latest',
        sourceType: 'module',
      },
    },

    plugins: {
      '@typescript-eslint': pluginTypescript,
      'check-file': pluginCheckFile,
    },

    rules: {
      '@typescript-eslint/no-unused-vars': 'error',
      '@typescript-eslint/no-explicit-any': 'error',
      'check-file/filename-naming-convention': [
        'error',
        { '**/*.{js,ts}': 'CAMEL_CASE' },
        { ignoreMiddleExtensions: true },
      ],
    },
  },
  prettier,
];
